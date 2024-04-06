(ns nextjournal.command-bar
  (:require [clojure.string :as str]
            [nextjournal.command-bar.hooks :as hooks]
            [nextjournal.command-bar.fuzzy :as fuzzy]
            [nextjournal.command-bar.keybind :as keybind]
            [reagent.core :as reagent]))

(defonce !bindings (reagent/atom []))
(defonce !global-bindings (reagent/atom []))
(defonce !local-bindings (reagent/atom []))
(defonce !local-view (reagent/atom nil))
(defonce !state (reagent/atom {}))

(defn chord->spec [{:keys [shift ctrl meta alt button]}]
  (str/join "-" (remove nil? [(when meta "meta")
                              (when shift "shift")
                              (when ctrl "ctrl")
                              (when alt "alt")
                              button])))

(defn normalize-spec [spec]
  (chord->spec (keybind/parse-chord (str/lower-case spec))))

(defn mod-only-chord? [{:keys [button]}]
  (some #(str/starts-with? button %) (keys keybind/MODS)))

(defn get-fn-key [fvar]
  (keyword (symbol fvar)))

(defn get-binding-by-name [name]
  (first (filter #(= name (:name %)) @!bindings)))

(defn get-binding-by-spec [spec]
  (let [spec (normalize-spec spec)]
    (first (filter #(= spec (:spec %)) @!bindings))))

(defn global-set-key! [name {:keys [binding run]}]
  (let [spec (normalize-spec binding)]
    (keybind/bind! spec name (fn [event]
                               (.preventDefault event)
                               (run)))
    (swap! !global-bindings conj {:spec spec :name name :run run})))

(defn global-unset-key! [name {:keys [binding run]}]
  (when-let [{:keys [spec run]} (get-binding-by-name name)]
    (keybind/unbind! spec name)
    (swap! !global-bindings (fn [bindings]
                              (remove #(= name (:name %)) bindings)))))

(defonce !fn-names (atom {}))

(defn get-var-name [demunged-name]
  (let [parts (str/split demunged-name #"/")]
    (if (seq (rest parts))
      (str (str/join "." (drop-last parts)) "/" (last parts))
      demunged-name)))

(defn get-fn-name [f]
  ;; TODO: Expose ns too when not default commands, e.g. view/toggle-command-bar
  ;; TODO: Show binding only on first occurence of command, others don't show their (duplicate) bindings
  (let [n (.-name f)]
    (get @!fn-names n
         (let [fn-name (last (str/split (demunge n) #"/")) #_(get-var-name (demunge n))]
           (swap! !fn-names assoc n fn-name)
           fn-name))))

(defn use-watches []
  (hooks/use-effect (fn []
                      (let [reset-bindings! #(reset! !bindings (concat @!global-bindings @!local-bindings))]
                        (add-watch !global-bindings :global reset-bindings!)
                        (add-watch !local-bindings :codemirror reset-bindings!)
                        #(do (remove-watch !global-bindings :global)
                             (remove-watch !local-bindings :codemirror))))))

(defn get-pretty-spec [spec]
  (->> (str/split spec #"-")
       (map (fn [k]
              ;; TODO: Investigate why Cmd/Meta/Mod and Control/Ctrl are not unified
              (get {"alt" "⌥"
                    "option" "⌥"
                    "control" "^"
                    "ctrl" "^"
                    "shift" "⇧"
                    "cmd" "⌘"
                    "meta" "⌘"
                    "mod" "⌘"
                    "arrowleft" "←"
                    "arrowright" "→"
                    "arrowup" "↑"
                    "arrowdown" "↓"
                    "backspace" "⌫"
                    "enter" "↩"
                    "delete" "Del"} (str/lower-case k) (str/capitalize k))))
       (str/join " ")))

(defn run-binding [{:keys [local? run]}]
  (if local?
    (do
      (.focus @!local-view)
      (js/requestAnimationFrame #(run @!local-view)))
    (run)))

(defn kill-interactive! []
  (swap! !state dissoc :interactive :input/query :pick-list/filtered-items :pick-list/selected-index)
  (when-let [cm @!local-view]
    (.focus cm)))

(defn make-interactive! [interactive-fn]
  (swap! !state assoc :interactive interactive-fn))

(defn toggle-interactive! [interactive-fn]
  (if (:interactive @!state)
    (kill-interactive!)
    (make-interactive! interactive-fn)))

(defn cmd-view [{:keys [spec name selected? show-binding?]}]
  (let [!el (hooks/use-ref nil)]
    (hooks/use-effect #(when selected? (.scrollIntoView @!el)) [selected?])
    [:div.flex.items-center.flex-shrink-0.font-mono.gap-1.relative.transition
     {:class (str "text-[12px] h-[26px] " (if selected? "text-indigo-300" "text-white"))
      :ref !el}
     [:div name]
     (when show-binding?
       [:div.font-inter
        {:class (if selected? "text-indigo-300" "text-slate-300")}
        (get-pretty-spec spec)])
     [:div.absolute.bottom-0.left-0.w-full.transition.bg-indigo-300
      {:class (str "h-[2px] " (if selected? "opacity-100" "opacity-0"))}]]))

(defn pick-list-item-view [{:keys [label selected?]}]
  (let [!el (hooks/use-ref nil)]
    (hooks/use-effect #(when selected? (.scrollIntoView @!el)) [selected?])
    [:div.flex.items-center.flex-shrink-0.font-mono.gap-1.relative.transition
     {:class (str "text-[12px] h-[26px] " (if selected? "text-indigo-300" "text-white"))
      :ref !el}
     [:div label]
     [:div.absolute.bottom-0.left-0.w-full.transition.bg-indigo-300
      {:class (str "h-[2px] " (if selected? "opacity-100" "opacity-0"))}]]))

(defn handle-component-keys [keymap event]
  (doseq [[key f] keymap]
    (when (= (.-key event) key)
      (.preventDefault event)
      (f event))))

(def default-component-keys
  {"Escape" #(kill-interactive!)
   "Enter" #(kill-interactive!)})

(defn label [{:keys [text]}]
  [:label.text-white.mr-2.flex.items-center.font-mono {:class "h-[26px] text-[12px]"} text])

(defn input [!state {:keys [placeholder on-input on-blur on-key-down on-key-up component-keys default-value]}]
  (let [{:input/keys [query]} @!state]
    [:div.relative.flex-shrink-0.border-r.border-slate-600.pr-3.mr-3
     [:div.whitespace-no-wrap.font-mono.pointer-events-none
      {:class "text-[12px]"}
      (if (str/blank? query) placeholder query)]
     [:input.bg-transparent.font-mono.text-white.focus:outline-none.absolute.left-0.top-0.w-full
      {:autoFocus true
       :on-input on-input
       :on-blur on-blur
       :on-key-down (or on-key-down (partial handle-component-keys (merge default-component-keys component-keys)))
       :on-key-up (or on-key-up #())
       :class "text-[12px]"
       :type "text"
       :placeholder placeholder
       :default-value default-value}]]))

(defn pick-list [!state {:keys [placeholder items on-select fuzzy/match]}]
  (let [{:pick-list/keys [filtered-items selected-index] :input/keys [query]
         :or {selected-index 0}} @!state
        items* (if (str/blank? query)
                 items
                 (fuzzy/search items match query))]
    [:<>
     [:style ".cmd-list::-webkit-scrollbar { height: 0; } .cmd-list { scrollbar-width: none; }"]
     [input !state {:placeholder placeholder
                    :on-input (fn [event]
                                (swap! !state merge {:input/query (.. event -target -value)
                                                     :pick-list/selected-index 0
                                                     :pick-list/filtered-items items*}))
                    :on-blur #(swap! !state dissoc :input/query :pick-list/selected-index :pick-list/filtered-items)
                    :component-keys {"ArrowRight" (fn [] (swap! !state update :pick-list/selected-index #(min (dec (count items*)) (inc %))))
                                     "ArrowLeft" (fn [] (swap! !state update :pick-list/selected-index #(max 0 (dec %))))
                                     "Enter" #(do
                                                (swap! !state dissoc :input/query)
                                                (on-select (nth items* selected-index) %))}}]
     (into [:div.cmd-list.flex.flex-auto.items-center.gap-3.overflow-x-auto]
           (map-indexed (fn [i {:as item :keys [item/view]}]
                          [(or view pick-list-item-view) (assoc item :selected? (= i selected-index))]))
           (or filtered-items items))]))

(defn toggle-command-bar
  "Shows a searchable list of all available commands. Use ← and → to navigate or ↩ to run the selected command."
  []
  (toggle-interactive! (fn [!state]
                         [pick-list !state {:placeholder "Search for commands…"
                                            :fuzzy/match #(-> % :name name)
                                            :items (map
                                                    (fn [item]
                                                      (assoc item :item/view cmd-view))
                                                    (vals (into {}
                                                                (map (fn [{:as binding :keys [run]}]
                                                                       [run binding]))
                                                                @!bindings)))
                                            :on-select (fn [selected-binding _event]
                                                         (kill-interactive!)
                                                         (run-binding selected-binding))}])))

(def default-commands
  {:toggle-command-bar {:binding "Alt-x" :run toggle-command-bar :display? true}
   :kill-interactive {:binding "Ctrl-g" :run kill-interactive!}})

(defn view [commands]
  (let [!el (hooks/use-ref nil)
        all-commands (merge commands default-commands)
        {:keys [interactive]} @!state]
    (use-watches)
    (hooks/use-effect (fn []

                        (doseq [[name cmd] all-commands]
                          (global-set-key! name cmd))
                        #(doseq [[name cmd] all-commands]
                           (global-unset-key! name cmd))))
    [:div.bg-slate-950.px-4.flex.items-center
     {:ref !el}
     (if interactive
       [interactive !state]
       (into [:<>]
             (->> all-commands
                  (filter (comp :display? val))
                  (keep (comp get-binding-by-name key))
                  (map (fn [b] [:div.text-slate-300.mr-3 {:class "text-[12px]"}
                                [cmd-view (assoc b :show-binding? true)]])))))]))
