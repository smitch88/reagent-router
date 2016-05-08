(ns play-with-routing.routes
  (:import goog.History)
  (:require [clojure.string :as string]
            [clojure.walk :refer [keywordize-keys]]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [reagent.core :as r]))

(defonce history (History.))
(defonce router-state (r/atom {:prefix "#"
                               :history history
                               :location nil
                               :routes nil
                               :params nil
                               :components []}))

(declare DefaultRoute)

;; helpers

(def decode js/decodeURIComponent)

(defn- establish-history-hook!
  "Establishes a history listener. Multiple calls to this will not generate
  additional listeners. `hook-callback` can be provided to call a function when
  the history object sees a change in navigation."
  [hook-callback]
  (doto history
    (events/listen EventType/NAVIGATE hook-callback)
    (.setEnabled true)))

(defn aget-in [itm path]
  (reduce
    (fn [acc i]
      (aget acc (name i)))
    itm
    path))

(defn- uri-without-prefix
  [prefix uri]
  (string/replace (or uri "") (re-pattern (str "^" prefix)) ""))

(defn- uri-with-leading-slash
  "Ensures that the uri has a leading slash"
  [uri]
  (if (= "/" (first uri))
    uri
    (str "/" uri)))

(defn- parse-path
  "Parse a value from a serialized query-string key index. If the
  index value is empty 0 is returned, if it's a digit it returns the
  js/parseInt value, otherwise it returns the extracted index."
  [path]
  (let [index-re #"\[([^\]]*)\]*" ;; Capture the index value.
        parts (re-seq index-re path)]
    (map
      (fn [[_ part]]
        (cond
          (empty? part) 0
          (re-matches #"\d+" part) (js/parseInt part)
          :else part))
      parts)))

(defn- key-parse
  "Return a key path for a serialized query-string entry.
  Ex.
  (key-parse \"foo[][a][][b]\")
  ;; => (\"foo\" 0 \"a\" 0 \"b\")
  "
  [k]
  (let [re #"([^\[\]]+)((?:\[[^\]]*\])*)?"
        [_ key path] (re-matches re k)
        parsed-path (when path (parse-path path))]
    (cons key parsed-path)))

(defn- assoc-in-query-params
  "Like assoc-in but numbers in path create vectors instead of maps.
  Ex.
  (assoc-in-query-params {} [\"foo\" 0] 1)
  ;; => {\"foo\" [1]}
  (assoc-in-query-params {} [\"foo\" 0 \"a\"] 1)
  ;; => {\"foo\" [{\"a\" 1}]}
  "
  [m path v]
  (let [heads (fn [xs]
                (map-indexed
                  (fn [i _]
                    (take (inc i) xs))
                  xs))
        hs (heads path)
        m (reduce
            (fn [m h]
              (if (and (or (number? (last h)))
                       (not (vector? (get-in m (butlast h)))))
                (assoc-in m (butlast h) [])
                m))
            m
            hs)]
    (if (zero? (last path))
      (update-in m (butlast path) conj v)
      (assoc-in m path v))))

(defn- get-parameters
  "Extract a map of query parameters from a query string."
  [query-string]
  (let [parts (string/split query-string #"&")]
    (keywordize-keys (reduce
                       (fn [m part]
                         (let [[k v] (string/split part #"=" 2)]
                           (assoc-in-query-params m (key-parse k) (decode v))))
                       {}
                       parts))))

(defn- match-routes
  "Given a particular `route` perform a depth first search that finds the route
  we are interested in. If there is, send back the component tree as a vector of
  components and location. This could be in the form of a redirect also.
  If there is not, send back the index or default route.
  Returns a triplet [components, route, error]"
  [route all-routes]
  (let [matched (->> (reduce (fn [acc r]
                               (let [f (comp (juxt :path :component) second)
                                     [path component] (f r)]
                                 (if (= path route)
                                   (assoc acc path component)
                                   acc)))
                             {}
                             all-routes)
                     vals)]
    (if matched
      [route matched nil]
      (if-let [default-route (first (filter #(identical? (first %) DefaultRoute) all-routes))]
        (let [[_ {:keys [path component]}] default-route]
          [route [component] nil])
        [route nil :no-match]))))

;; Link

(defn- map->params
  [query]
  (let [params (map #(name %) (keys query))
        values (vals query)
        pairs (partition 2 (interleave params values))]
    (string/join "&" (map #(string/join "=" %) pairs))))

(defn- navigate!
  "Add a browser history entry. Updates window/location"
  [route query]
  (let [token (.getToken history)
        old-route (first (string/split token "?"))
        query-string (map->params (reduce-kv (fn [valid k v]
                                               (if v
                                                 (assoc valid k v)
                                                 valid)) {} query))
        with-params (if (empty? query-string)
                      route
                      (str route "?" query-string))]
    (if (= old-route route)
      (. history (replaceToken with-params))
      (. history (setToken with-params)))))

(defn Link
  [{:keys [display href params] :as props
    :or {params {}}}]
  [:a.routed-link {:href href
                   :on-click (fn [e]
                               (.preventDefault e)
                               (navigate! href params))}
   display])

;; Route

(defn Route
  "A route component is just a wrapper so we can define the 'Router'
  configuration as a set of components/children of a higher-order component."
  [{:keys [path component] :as props} & children]
  false)

;; DefaultRoute

(defn DefaultRoute
  [props & children]
  [Route props children])

;; RouteComponentTree

(defn- RouteComponentTree
  "Renders the component tree for a given state passing in proper props which
  would be session + additional props defined by routing parameters."
  [init-props]
  (r/create-class
    {:reagent-render
     (fn [{:keys [components routes location params] :as router-state}]
       (into [:div {:style {:height "inherit" :width "inherit"}}]
             (mapv #(vector % params) components)))}))

;; Router

(defn- handle-route-change
  [{:keys [route params on-error on-update] :as props} routes]
  (let [[location components error] (match-routes route routes)]
    (if error
      (do
        (js/console.info "route change error occurred " error)
        ;; call user defined callback if passed in
        (when (and on-error error)
          (on-error error)))
      (do
        (swap! router-state assoc
               :location location
               :components components
               :params params)
        (navigate! location params)))))

(defn transition-listener
  "Manages route changes received from the listener."
  [{:keys [prefix on-error on-update]} routes]
  (fn [e]
    (let [{:keys [location]} @router-state
          uri (aget e "token")
          [uri-path query-string] (string/split (uri-without-prefix prefix uri) #"\?")
          uri-path (uri-with-leading-slash uri-path)
          query-parameters (get-parameters query-string)]
      (when-not (= uri location)
        (handle-route-change {:route uri-path
                              :params query-parameters
                              :on-error on-error
                              :on-update on-update}
                             routes)))))

(defn Router
  "High-order component that emits a `RouterImpl` component. The router strictly
  handles transition management and updates url history context.
  This is specfically hash based."
  [{:keys [on-error on-update] :as props} & routes]
  (r/create-class
    {:component-will-mount
     (fn []
       (let [hash (-> js/window .-location .-hash)]
         ;; set up history listener
         (establish-history-hook! (transition-listener props routes))
         (navigate! (uri-without-prefix "#" hash) {})))
     :reagent-render
     (fn []
       ;; generate component tree based on the state
       [RouteComponentTree @router-state])}))
