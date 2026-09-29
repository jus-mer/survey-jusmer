# Zero-sum allocation widget: ONE bar, THREE draggable dividers, FOUR segments
# whose widths (in %) always sum to 100.
#
# id:     surveydown question id (a hidden text question is created for it).
#         Stored value is "e,t,f,c" (percentages in `keys` order), or empty
#         until the respondent touches the widget. Also stored:
#         {id}_answered (0 = untouched, 1 = answered). Split the string into the
#         four percentages (order of `labels`) at analysis time.
# labels: named character vector, names = keys used in the data, values = text shown
# colors: one colour per segment
# step:   grid the dividers snap to (percentage points)
zero_sum_widget <- function(
    id,
    labels = c(effort = "Esfuerzo", talent = "Talento", family = "Familia", connections = "Contactos"),
    colors = c("#0072B2", "#E69F00", "#009E73", "#CC79A7"),
    step = 5,
    start = c(25, 50, 75)) {

  stopifnot(length(labels) == 4, length(colors) == 4, length(start) == 3)
  cfg <- jsonlite::toJSON(
    list(id = id, keys = names(labels), labels = unname(labels),
         colors = colors, step = step, start = start,
         points = seq(0, 100, by = step)),
    auto_unbox = TRUE
  )

  segs <- lapply(1:4, function(i) shiny::div(
    class = "zs-seg", `data-i` = i, style = paste0("background:", colors[i], ";"),
    shiny::span(class = "zs-seg-pct", "25%")
  ))
  hands <- lapply(1:3, function(i) shiny::div(
    class = "zs-handle", `data-i` = i, tabindex = 0, role = "slider",
    `aria-label` = paste("Divisor", i), `aria-valuemin` = 0, `aria-valuemax` = 100
  ))
  boxes <- lapply(1:4, function(i) shiny::div(
    class = "zs-box", `data-i` = i, style = paste0("border-color:", colors[i], ";"),
    shiny::div(class = "zs-box-fill", style = paste0("background:", colors[i], ";")),
    shiny::div(
      class = "zs-box-body",
      shiny::div(class = "zs-box-name", style = paste0("color:", colors[i], ";"), labels[[i]]),
      shiny::div(class = "zs-box-pct", "25%")
    )
  ))

  htmltools::browsable(htmltools::tagList(
    # Hidden surveydown question that carries the value
    shiny::div(
      class = "zs-hidden",
      sd_question(type = "text", id = id, label = ""),
      # explicit answered flag (0/1)
      sd_question(type = "text", id = paste0(id, "_answered"), label = "")
    ),
    shiny::div(
      id = paste0("zs-", id), class = "zs-widget",
      shiny::div(class = "zs-track", segs, hands),
      shiny::div(class = "zs-boxes", boxes),
      shiny::div(class = "zs-total", shiny::span(class = "zs-total-txt", "Total: 100%"))
    ),
    shiny::tags$style(htmltools::HTML("
      .zs-hidden { display: none !important; }
      .zs-widget { max-width: 640px; margin: 8px auto 12px; user-select: none; }
      .zs-track { position: relative; display: flex; height: 56px; border-radius: 8px;
                  overflow: visible; touch-action: none; margin: 18px 0 22px; }
      .zs-seg { display: flex; align-items: center; justify-content: center; min-width: 0;
                overflow: hidden; color: #fff; font-weight: 800; font-size: .9rem;
                transition: width 60ms linear; }
      .zs-seg:first-child { border-radius: 8px 0 0 8px; }
      .zs-seg:nth-child(4) { border-radius: 0 8px 8px 0; }
      .zs-handle { position: absolute; top: -8px; height: 72px; width: 22px; margin-left: -11px;
                   background: #ffd43b; border: 2px solid #e0a800; border-radius: 8px;
                   cursor: ew-resize; touch-action: none; z-index: 2;
                   box-shadow: 0 0 0 2px rgba(255,212,59,.28); }
      .zs-handle:focus { outline: 3px solid #333; }
      .zs-handle::after { content: ''; position: absolute; left: 50%; top: 20%; bottom: 20%;
                          width: 0; border-left: 2px solid #8a6d00; border-right: 2px solid #8a6d00;
                          transform: translateX(-50%); width: 4px; }
      .zs-boxes { display: flex; gap: 8px; }
      .zs-box { flex: 1; min-width: 0; position: relative; overflow: hidden; text-align: center;
                border: 2px solid; border-radius: 6px; padding: 8px 2px; background: #fff; }
      .zs-box-fill { position: absolute; left: 0; right: 0; bottom: 0; height: 25%; opacity: .18;
                     pointer-events: none; transition: height 120ms linear; }
      .zs-box-body { position: relative; }
      .zs-box-name { font-weight: 800; font-size: .9rem; }
      .zs-box-pct { font-weight: 900; font-size: 1.2rem; color: #212529; }
      .zs-total { text-align: center; margin-top: 8px; color: #6c757d; font-size: .85rem; }
      .zs-untouched .zs-seg { background: #ced4da !important; }
      .zs-untouched .zs-seg-pct { display: none; }
      .zs-untouched .zs-box-fill { display: none; }
      .zs-untouched .zs-box-pct { color: #adb5bd; }
      .zs-widget.zs-untouched .zs-total-txt { display: none; }
      .zs-widget.zs-untouched .zs-total::before { content: 'Sin responder: mueva los divisores para responder'; color: #dc3545; font-weight: 600; }
      @media (max-width: 576px) { .zs-seg-pct { display: none; } .zs-box-name { font-size: .75rem; } }
    ")),
    shiny::tags$script(htmltools::HTML(paste0("
    (function() {
      var cfg = ", cfg, ";
      var root = document.getElementById('zs-' + cfg.id);
      if (!root || root.dataset.init) return;
      root.dataset.init = '1';
      var track = root.querySelector('.zs-track');
      var handles = [].slice.call(root.querySelectorAll('.zs-handle'));
      var segs = [].slice.call(root.querySelectorAll('.zs-seg'));
      var boxes = [].slice.call(root.querySelectorAll('.zs-box'));
      var pos = cfg.start.slice();
      var touched = false;
      root.classList.add('zs-untouched');

      function snap(v) {
        return cfg.points.reduce(function(best, p) { return Math.abs(p - v) < Math.abs(best - v) ? p : best; });
      }
      function neighbour(v, dir) {
        var c = cfg.points.filter(function(p) { return dir > 0 ? p > v : p < v; });
        return c.length ? (dir > 0 ? c[0] : c[c.length - 1]) : v;
      }
      function parts() { return [pos[0], pos[1] - pos[0], pos[2] - pos[1], 100 - pos[2]]; }

      function render() {
        var p = parts();
        segs.forEach(function(s, i) {
          s.style.width = p[i] + '%';
          s.querySelector('.zs-seg-pct').textContent = p[i] > 0 ? p[i] + '%' : '';
        });
        boxes.forEach(function(b, i) {
          b.querySelector('.zs-box-pct').textContent = touched ? p[i] + '%' : '\u2014';
          b.querySelector('.zs-box-fill').style.height = p[i] + '%';
        });
        handles.forEach(function(h, i) {
          h.style.left = pos[i] + '%';
          h.setAttribute('aria-valuenow', pos[i]);
        });
      }

      function setVal(id, v) {
        var input = document.getElementById(id);
        if (!input || input.value === v) return;
        input.value = v;
        input.dispatchEvent(new Event('input', { bubbles: true }));
        input.dispatchEvent(new Event('change', { bubbles: true }));
      }

      function push() {
        var p = parts();
        setVal(cfg.id, touched ? p.join(',') : '');
        setVal(cfg.id + '_answered', touched ? '1' : '0');
      }

      function move(i, v) {
        v = Math.max(i > 0 ? pos[i - 1] : 0, Math.min(i < 2 ? pos[i + 1] : 100, v));
        if (v === pos[i] && touched) return;
        pos[i] = v;
        if (!touched) { touched = true; root.classList.remove('zs-untouched'); }
        render(); push();
      }

      var drag = null;
      handles.forEach(function(h, i) {
        h.addEventListener('pointerdown', function(e) {
          e.preventDefault();
          h.setPointerCapture(e.pointerId);
          // dividers stacked at the same spot: decide which one by drag direction
          var group = [i];
          for (var j = 0; j < 3; j++) if (j !== i && pos[j] === pos[i]) group.push(j);
          group.sort();
          drag = { i: i, group: group, x0: e.clientX, decided: group.length === 1, h: h };
        });
        h.addEventListener('pointermove', function(e) {
          if (!drag || drag.h !== h) return;
          var rect = track.getBoundingClientRect();
          var v = snap((e.clientX - rect.left) / rect.width * 100);
          if (!drag.decided) {
            if (e.clientX === drag.x0) return;
            drag.i = e.clientX > drag.x0 ? drag.group[drag.group.length - 1] : drag.group[0];
            drag.decided = true;
          }
          move(drag.i, v);
        });
        function end() { drag = null; }
        h.addEventListener('pointerup', end);
        h.addEventListener('pointercancel', end);
        h.addEventListener('keydown', function(e) {
          var d = e.key === 'ArrowLeft' ? -1 : e.key === 'ArrowRight' ? 1 : 0;
          if (d) { e.preventDefault(); move(i, neighbour(pos[i], d)); }
        });
      });

      render();
      setTimeout(push, 600);  // record answered = 0 for untouched widgets
    })();
    ")))
  ))
}
