(ns nextjournal.command-bar
  (:require ["@codemirror/state" :refer [StateField]]
            ["@codemirror/view" :refer [keymap]]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [nextjournal.clerk.render.hooks :as hooks]
            [nextjournal.command-bar.fuzzy :as fuzzy]
            [reagent.core :as reagent]))

(defonce !bindings (reagent/atom []))
(defonce !global-bindings (reagent/atom []))
(defonce !codemirror-bindings (reagent/atom []))

(defn global-unset-key! [keybinding]
  (swap! !global-bindings (fn [bindings]
                            (remove #(= keybinding (j/get % :key)) bindings))))

(defn global-set-key! [keybinding f]
  (swap! !global-bindings conj #js {:key keybinding :run f}))

(defn get-global-binding [key]
  (first (filter #(= key (j/get % :key)) @!global-bindings)))

(defn get-codemirror-bindings [^js state]
  (.filter
   (.flat (.facet state keymap))
   (fn [binding]
     (let [{:keys [key mac run]} (j/lookup binding)]
       (and (or key mac) run (.-name run)))))) ;; TODO: removing everything that doesn't have a fn name for now. Let's revisit.

(def extension
  (.-extension (.define StateField
                        (j/lit {:create #(reset! !codemirror-bindings (get-codemirror-bindings %))
                                :update (fn [_value tr]
                                          #(reset! !codemirror-bindings (get-codemirror-bindings (.-state tr))))}))))

(defonce modifiers #{"Alt" "Control" "Meta" "Shift"})

(defn get-event-modifier [event]
  (cond
    (.-metaKey event) "Meta"
    (.-altKey event) "Alt"
    (.-ctrlKey event) "Control"
    (.-shiftKey event) "Shift"))

(defn use-global-keybindings []
  (hooks/use-effect
   (fn []
     (let [handle-global-key (fn [event]
                               (when-not (contains? modifiers (.-key event))
                                 (let [modifier (get-event-modifier event)
                                       key (cond->> (-> (.-code event) (str/replace #"Key" "") str/lower-case)
                                             modifier (str modifier "-"))]
                                   (when-let [binding (get-global-binding key)]
                                     (.preventDefault event)
                                     (j/call binding :run)))))]
       (.addEventListener js/document "keypress" handle-global-key)
       #(.removeEventListener js/document "keypress" handle-global-key)))))

(defn use-watches []
  (hooks/use-effect (fn []
                      (let [reset-bindings! #(reset! !bindings (concat @!global-bindings @!codemirror-bindings))]
                        (add-watch !global-bindings :global reset-bindings!)
                        (add-watch !codemirror-bindings :codemirror reset-bindings!)
                        #(do (remove-watch !global-bindings :global)
                             (remove-watch !codemirror-bindings :codemirror))))))

(defn get-fn-name [f]
  (-> (str/split (.-name f) #"\$[_]") last (str/replace #"_" "-")))

(defn cmd-view [binding]
  (let [{:keys [key mac run]} (j/lookup binding)
        fn-name (.-name run)]
    [:div.flex.items-center.flex-shrink-0.font-inter.gap-1.text-white
     {:class "text-[12px]"}
     [:div (get-fn-name run)]
     [:div.text-slate-300 (or key mac)]]))

(defn view []
  (let [!el (hooks/use-ref nil)
        !focus? (hooks/use-state false)
        !query (hooks/use-state nil)]
    (use-global-keybindings)
    (use-watches)
    (hooks/use-effect (fn []
                        (let [toggle-command-bar (fn toggle-command-bar []
                                                   (swap! !focus? not))]
                          (global-set-key! "Alt-x" toggle-command-bar)
                          #(global-unset-key! "Alt-x"))))
    [:div.bg-slate-950.px-4.overflow-x-auto.flex.items-center
     {:ref !el :class "h-[26px]"}
     (if @!focus?
       [:<>
        (let [placeholder "Search for commands…"]
          [:div.relative.flex-shrink-0.border-r.border-slate-600.pr-3.mr-3
           [:div.whitespace-no-wrap.font-inter.pointer-events-none
            {:class "text-[12px]"}
            (if (str/blank? @!query) placeholder @!query)]
           [:input.bg-transparent.font-inter.text-white.focus:outline-none.absolute.left-0.top-0.w-full.h-full
            {:autoFocus true
             :on-input #(reset! !query (.. % -target -value))
             :on-blur #(reset! !query nil)
             :class "text-[12px]"
             :type "text"
             :placeholder placeholder}]])
        (into [:div.flex.items-center.gap-3]
              (map cmd-view)
              (fuzzy/search @!bindings #(get-fn-name (j/get % :run)) (or @!query "")))]
       [:div.text-slate-300 {:class "text-[12px]"}
        (when-let [binding (get-global-binding "Alt-x")]
          [cmd-view binding])])]))

