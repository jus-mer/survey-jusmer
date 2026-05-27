// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  pagenumbering: "1",
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: pagenumbering,
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black or heading-decoration == "underline"
           or heading-background-color != none) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#show: doc => article(
  title: [Merecimiento y Justicia de Mercado],
  subtitle: [Concepciones, medición y factores asociados],
  authors: (
    ( name: [René Canales],
      affiliation: [],
      email: [] ),
    ( name: [Juan Carlos Castillo],
      affiliation: [],
      email: [] ),
    ),
  abstract: [El presente documento de trabajo tiene como objetivo sistematizar la conceptualización del merecimiento, revisar las estrategias metodológicas para su medición, y analizar los factores determinantes que moldean estas percepciones, integrando tanto la literatura sobre bienestar como los nuevos hallazgos sobre desigualdad y acumulación de riqueza.

],
  abstract-title: "Abstract",
  paper: "letter",
  font: ("Times New Roman",),
  fontsize: 13pt,
  pagenumbering: "1",
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Introducción
<introducción>
En un contexto marcado por crisis económicas, aumento de la desigualdad y transformaciones demográficas, la pregunta sobre "¿quién recibe qué y por qué?" ha retornado con fuerza a la agenda pública y académica, situándose en el centro del debate sobre política social y de los estudios sobre percepciones y preferencias de la desigualdad @meuleman_welfare_2020. Más cuando gran parte de la legitimidad de los Estados de bienestar y de sus procesos de transformación no depende únicamente de la eficiencia económica o del diseño institucional, sino que está profundamente anclada en la percepción ciudadana sobre la justicia distributiva y la solidaridad (Brooks & Manza, 2006). Sin embargo, estas percepciones a menudo divergen de la realidad, lo que revela una brecha significativa entre los indicadores económicos y la forma en que los ciudadanos perciben y evalúan las desigualdades multidimensionales @kulic_comprehensive_2025.

En este marco, el concepto de merecimiento emerge como una herramienta analítica clave para comprender por qué el apoyo público varía de manera tan significativa según el tipo de beneficiario, tanto en la provisión de ayuda social como en el acceso a distintos bienes en diversas áreas @vanoorschot_social_2017. Las teorías del merecimiento postulan que la solidaridad es condicional: los ciudadanos no apoyan la redistribución de manera abstracta, sino que distinguen entre grupos "merecedores" y "no merecedores" con base en criterios morales y en evaluaciones de justicia @gift_deservingness_2023. La evidencia reciente refina esta distinción, sugiriendo que los ciudadanos aplican una suerte de evaluación responsable mediante la cual tienden a compensar la mala "suerte bruta" (fuera del control individual), pero penalizan la derivada de la "suerte opcional" o de riesgos asumidos @chancel_world_2022.

La literatura se ha centrado en el estudio de los criterios de merecimiento de los desfavorecidos; investigaciones recientes han ampliado este enfoque hacia el "merecimiento de la riqueza", cuestionando la legitimidad de las fortunas acumuladas y su tributación @becker_taxing_2025@sachweh_deserving_2023@mccall_undeserving_2013. Por lo mismo, resulta crucial considerar que las propias creencias sobre el merecimiento son endógenas al estatus económico: el éxito sesgaría las percepciones individuales, llevando a los "ganadores" a atribuir su posición al esfuerzo y a subestimar el rol de la suerte, legitimando así niveles más altos de desigualdad @deng_its_2025@fehr_misperceiving_2025.

A pesar de la relevancia del concepto de merecimiento, persiste cierta confusión en su medición. A menudo, el merecimiento se ha tratado como un heurístico no medido o se ha inferido a partir de preferencias de política pública @meuleman_welfare_2020@vanoorschot_social_2017, en lugar de operacionalizarse como un constructo multidimensional con escalas validadas (Meuleman et al., 2020) o mediante nuevos módulos de encuesta capaces de distinguir con precisión entre aversión a la desigualdad y altruismo (Epper & Mitrouchev, 2025).

El presente documento de trabajo tiene como objetivo sistematizar la conceptualización del merecimiento, revisar las estrategias metodológicas para su medición y analizar los factores determinantes que moldean estas percepciones, integrando tanto la literatura sobre bienestar como los nuevos hallazgos sobre desigualdad y acumulación de riqueza.

= Conceptualización del merecimiento
<conceptualización-del-merecimiento>
== La estructura moral del merecimiento
<la-estructura-moral-del-merecimiento>
Hay que comenzar recordando que el merecimiento se entiende como aquellas evaluaciones que las personas realizan sobre si se merecen o no ciertos beneficios, castigos o reconocimientos @oorschot_who_2000. Esto es especialmente relevante en la literatura sobre "policy feedback effects" de los Estados de bienestar y de mercantilización @busemeyer_policy_2020@lindh_public_2015, pues hace referencia específica a lo que las personas evalúan como justo en el acceso, la distribución o mercantilización de esos servicios desde una economía moral.

La conceptualización del merecimiento ha evolucionado desde ser considerada una intuición vaga hasta establecerse como una estructura cognitiva compleja. Van Oorschot et al. @vanoorschot_social_2017@oorschot_who_2000 sostienen que la solidaridad pública no es ciega, sino que está fuertemente condicionada por criterios normativos que la ciudadanía aplica para juzgar a los beneficiarios de la protección social. Este marco teórico se basa en el modelo Control, Attitude, Reciprocity, Identity, and Need (CARIN), que postula que las evaluaciones de merecimiento se construyen a partir de estas cinco dimensiones interrelacionadas:

- Control: se refiere a la percepción de la responsabilidad personal ante la situación de necesidad. Según Meuelemann @meuleman_welfare_2020, esta dimensión es a menudo la más determinante, pues aquellos cuya necesidad es percibida como ajena a su voluntad ---como los ancianos o los enfermos--- gozan de mayor legitimidad en términos de merecimiento que quienes son considerados responsables de su propia situación, lo que penaliza sistemáticamente a los desempleados. La investigación reciente ha refinado esta dimensión, sugiriendo que la ciudadanía aplica un "corte de responsabilidad" que distingue no solo el control general, sino también el tipo de azar involucrado: se tiende a compensar la "suerte bruta" (eventos incontrolables), pero no la "suerte opcional" derivada de riesgos asumidos @chanel_preferences_2025. Además, esta percepción de control está sujeta a sesgos cognitivos importantes; por ejemplo, el éxito económico propio lleva a los individuos a sobreestimar el papel del esfuerzo y subestimar el de la suerte, lo que distorsiona sus juicios sobre quién merece qué @deng_its_2025@fehr_misperceiving_2025@gugushvili_what_2026.

- Actitud: evalúa la #strong[disposición] del beneficiario hacia la sociedad y su #strong[gratitud] por la ayuda recibida, mientras que la Reciprocidad mide si el individuo ha contribuido previamente al sistema (mediante impuestos o trabajo) o si tiene potencial para hacerlo en el futuro.

- Recipricidad: (?)

- Identidad: delimita las fronteras morales de la comunidad, favoreciendo a los miembros del propio grupo (ingroup) sobre quienes están fuera (outgroup). Esto explica las brechas recurrentes de apoyo hacia ciertos grupos, como la población inmigrante, una tendencia que persiste incluso en contextos donde se reconoce la existencia de desigualdades estructurales severas @kulic_comprehensive_2025.

- Finalmente, la dimensión de Necesidad se centra en la urgencia y la magnitud de la situación del beneficiario.

La operacionalización de estos conceptos plantea desafíos metodológicos importantes. Como señalan Meuleman et al. @meuleman_welfare_2020, gran parte de la investigación aborda el merecimiento como un concepto heurístico, infiriendo las opiniones de los ciudadanos a partir de sus preferencias por políticas concretas, lo que genera confusión conceptual entre la evaluación moral de un grupo y el apoyo ideológico al Estado de bienestar @vanoorschot_social_2017.

Para superar esta limitación, Meuleman et al. @meuleman_welfare_2020 desarrollaron y validaron una #strong[Escala de Principios de Merecimiento CARIN];, demostrando que los cinco criterios son dimensiones distintas que pueden operar de manera independiente o incluso contradictoria en la evaluación moral de ciertos contextos. Este avance permite investigar cómo la adhesión a principios abstractos de merecimiento tiene un poder predictivo significativo sobre las actitudes hacia políticas de bienestar específicas. Más recientemente, la innovación metodológica ha dado un paso adicional con el desarrollo de módulos de encuesta validados, como el de Epper y Mitrouchev @epper_measuring_2025, que permiten no solo medir el merecimiento @renejcanales[cómo mide esto merecimiento? creo que no está relacionado con esta sección, el paper ni menciona merecimiento], sino desagregar empíricamente las motivaciones subyacentes, distinguiendo con precisión entre la aversión a la desigualdad (un principio de justicia) y el altruismo (una preocupación por el bienestar ajeno), validando empíricamente la teoría de que los juicios morales preceden y moldean las preferencias políticas.

== La expansión del concepto como heurístico: el merceimiento de las élites y la riqueza @renejcanales
<la-expansión-del-concepto-como-heurístico-el-merceimiento-de-las-élites-y-la-riqueza-esta-sección-no-se-relaciona-con-el-objetivo-del-documento-renejcanales>
Tradicionalmente, el estudio del merecimiento se ha centrado en los sectores más desfavorecidos de la sociedad ---los undeserving poor---, pero la creciente concentración de capital ha obligado a la investigación a ampliar su enfoque hacia quienes se benefician en mayor medida de este proceso, es decir, los undeserving rich @mccall_undeserving_2013. En el plano de las percepciones y preferencias, este giro se ha abordado en la literatura sobre meritocracia, donde el trabajo duro y el talento se conciben como los principales determinantes del éxito económico. El hallazgo central es claro: quienes creen firmemente en la meritocracia muestran menor preocupación por la distribución y la desigualdad @friedman_meaning_2024@mijs_paradox_2021. Sin embargo, evidencia reciente sugiere que esta creencia no es neutral, sino que es endógena al propio estatus: Fehr y Vollmann @fehr_misperceiving_2025 demuestran que el éxito económico induce un "sesgo de merecimiento", llevando a los individuos exitosos a atribuir sus logros al esfuerzo y a subestimar sistemáticamente el rol de la suerte, lo que les permite justificar moralmente mayores niveles de desigualdad.

En esta línea, y siguiendo los criterios de CARIN, la literatura también ha estudiado cómo influyen los factores que escapan al control individual. En un estudio reciente, Becker y Waitkus @becker_taxing_2025 descubrieron que el origen de la riqueza es un factor fundamental para su legitimidad social. A través de experimentos de encuestas en Alemania, los autores demuestran que la riqueza heredada, particularmente aquella vinculada a orígenes históricos inmorales o ilegítimos ---como la cooperación con el régimen nazi---, genera una oposición significativamente mayor y demandas más fuertes de redistribución que la riqueza percibida como fruto del emprendimiento propio. Esto sugiere que el merecimiento es una cualidad heredable y transgeneracional: la "mancha" moral de la acumulación original puede deslegitimar a los herederos, activando una demanda punitiva de impuestos que va más allá de la simple envidia de clase o el igualitarismo abstracto.

Complementariamente, la legitimación de la riqueza autogenerada también varía según las narrativas culturales sobre el mérito. Friedman et al. @friedman_meaning_2024 exploran cómo las élites construyen su propio merecimiento, encontrando diferencias sustanciales entre contextos nacionales. Mientras que en el Reino Unido las élites tienden a legitimar su posición mediante una retórica del "talento" innato y la brillantez excepcional, en países como Dinamarca predomina una narrativa centrada en el "trabajo duro" y el esfuerzo ordinario. Estas diferencias no son triviales: la justificación basada en el talento tiende a naturalizar la desigualdad como un hecho biológico o inevitable, lo que dificulta cuestionar la jerarquía social, mientras que la retórica del esfuerzo, aunque meritocrática, mantiene la desigualdad dentro del terreno de la acción humana. Esto, con resultados similares, es lo que han encontrado trabajos como el de Atria et al. @atria_economic_2020, quienes por medio de una serie de entrevistas semiestructuradas a accionistas y ejecutivos de alto nivel en empresas chilenas dieron cuenta de que la elite económica tiende al apoyo hacia la meritocracia, pero explican el ascenso a partir del talento en habilidades de liderazgo y negocios, antes que del esfuerzo y el trabajo duro.

Finalmente, la interacción entre principios morales y el beneficio personal añade una capa de complejidad. Sachweh y Eicher @sachweh_deserving_2023 indican que, si bien los principios de merecimiento son importantes, el autointerés subjetivo ---la percepción de la propia posición económica--- a menudo prevalece sobre las consideraciones normativas al apoyar impuestos a la riqueza. Esta tensión es corroborada por Chanel et al. @chanel_preferences_2025, quienes identifican experimentalmente la existencia de un "voto egoísta" (self-serving vote): aunque los ciudadanos suelen aplicar principios de justicia como el "corte de Dworkin" (compensar la mala suerte bruta pero no la suerte opcional derivada de riesgos asumidos), esta moralidad se ve comprometida cuando la redistribución entra en conflicto directo con la maximización de las ganancias personales.

== El merecimiento entre el contexto y la universalidad @renejcanales
<el-merecimiento-entre-el-contexto-y-la-universalidad-esta-sección-no-se-relaciona-con-el-objetivo-del-documento-renejcanales>
Algo que la literatura reciente ha confirmado es que las percepciones sobre el merecimiento son altamente sensibles a la información disponible sobre la estructura social y la desigualdad, aunque este procesamiento no es neutral. Mijs y Hoy @mijs_how_2022, por ejemplo, demostraron que la provisión de información sobre la magnitud real de la desigualdad y la falta de movilidad social puede afectar las creencias en la meritocracia @renejcanales, pero este efecto no es uniforme. En un estudio experimental comparativo, observaron que, mientras en países como Indonesia la información sobre la desigualdad socavaba la fe en el sistema, en contextos como Australia o México los efectos eran más limitados o mediados por el posicionamiento ideológico previo. Paralelamente, Den y Wang @deng_its_2025, a través de una serie de estudios experimentales y observacionales, demuestran que la exposición a historias de éxito individual incrementa la creencia de que el esfuerzo personal es el principal determinante del logro socioeconómico, incluso cuando las oportunidades estructurales son limitadas. Este fortalecimiento de la meritocracia descriptiva por medio de narrativas de éxito individual, opera como un mecanismo de justificación del sistema, promoviendo una mayor aceptación del orden social existente, una mayor atribución de culpa individual al fracaso y una mayor disposición a legitimar prácticas de explotación laboral. Pues, contribuirían a despolitizar la desigualdad y a sostener el statu quo al desplazar la atención desde las condiciones estructurales hacia la responsabilidad individual.

Sin embargo, la recepción de esta información enfrenta barreras cognitivas. Fehr y Vollmann @fehr_misperceiving_2025 identifican un mecanismo de "ignorancia motivada" en las élites: las personas exitosas muestran una disposición significativamente menor a recibir información veraz sobre las causas de su éxito (suerte vs.~esfuerzo) en comparación con las no exitosas. Esto sugiere que los "ganadores" del sistema evitan activamente los datos que podrían amenazar su autoimagen meritocrática, bloqueando así el efecto correctivo de la información sobre sus preferencias redistributivas.

Esto resuena con hallazgos como los de McCall et al. @mccall_exposure_2017, quienes argumentan que la exposición a información sobre la creciente desigualdad no siempre conduce a un mayor apoyo a la redistribución tradicional. En el caso estadounidense, cuando los ciudadanos perciben que las oportunidades estructurales están bloqueadas ("the opportunity gap"), su preocupación se centra más en la injusticia del proceso que en la desigualdad de resultados. No obstante, Chanel et al. @chanel_preferences_2025 matizan esto demostrando que el tipo de información es crucial: cuando se expone a los ciudadanos a la distribución real de las ganancias (mostrando la desigualdad agregada), se observa un desplazamiento significativo desde posturas meritocráticas hacia el igualitarismo (process-independent views), aumentando el apoyo a la redistribución incluso entre quienes previamente favorecían la igualdad de oportunidades.

En línea con lo anterior, Molina et al. @molina_its_2019, mediante un experimento causal para evaluar los efectos de la desigualdad de oportunidades y de resultados, muestran que las percepciones de justicia distributiva dependen no sólo de cómo se estructura el "juego", sino fundamentalmente de si las personas ganan o pierden. Utilizando un diseño experimental que manipula exógenamente la distribución de oportunidades sin involucrar diferencias reales de habilidad o esfuerzo, los autores encuentran que los ganadores son sistemáticamente más propensos que los perdedores a considerar los resultados como justos, a atribuir el éxito al talento y a expresar satisfacción personal, incluso cuando las reglas favorecen explícitamente su posición. Si bien un aumento en la redistribución de oportunidades reduce, tanto para ganadores como para perdedores, la percepción de justicia y la atribución a la habilidad, las diferencias normativas y afectivas entre ambos grupos persisten. De esta forma, las creencias sobre equidad y merecimiento están fuertemente mediadas por la posición relativa en la distribución de resultados, revelando un sesgo autojustificatorio que contribuye a la legitimación de desigualdades incluso cuando estas no pueden atribuirse a méritos individuales.

Hallazgos como estos permiten preguntarse qué sucede con la lógica del merecimiento cuando se trasciende la focalización y se mira a la universalidad. Gift y Lastra-Anadón @gift_deservingness_2023 evidencian que el apoyo a bienes públicos universales (como la salud pública) no es incondicional, sino que disminuye cuando se percibe que los beneficiarios carecen de la motivación adecuada para contribuir. Esto indica que la heurística del merecimiento es tan robusta que puede erosionar la legitimidad de políticas diseñadas precisamente para ser universales y ajenas al juicio moral individual.

Es crucial destacar que la lógica del merecimiento permea incluso aquellos espacios diseñados para ser inmunes a ella: los bienes públicos universales. Gift y Lastra-Anadón @gift_deservingness_2023 desafían la asunción de que la universalidad elimina el juicio moral, demostrando, mediante experimentos realizados en el Reino Unido, que el apoyo a instituciones sagradas como el Servicio Nacional de Salud (NHS) disminuye significativamente cuando se percibe que los usuarios carecen de "motivación" para contribuir o trabajar.

Este hallazgo indica que la distinción entre merecedores y no merecedores es un mecanismo cognitivo tan robusto que se activa incluso ante derechos universales. La literatura reciente permite desentrañar la mecánica de esta decisión: Chanel et al. @chanel_preferences_2025 demuestran que, en contextos de votación democrática, las "opiniones sobre la justicia" (fairness views) tienden a prevalecer sobre el puro autointerés económico. Los ciudadanos no penalizan indiscriminadamente, sino que aplican criterios precisos para retirar el apoyo a quienes perciben responsables de su situación (suerte opcional), manteniendo la protección para quienes sufren infortunios incontrolables (suerte bruta).

En este sentido, la disposición a sacrificar el propio acceso ideal a bienes públicos para no subsidiar a "polizones" (free-riders) no debe interpretarse necesariamente como una falta de generosidad. Epper y Mitrouchev @epper_measuring_2025 sugieren que la aversión a la desigualdad (el disgusto por las disparidades injustas) y el altruismo son motivaciones distintas; por tanto, un ciudadano puede ser altruista y, simultáneamente, retirar su apoyo a un bien universal si percibe que su distribución viola principios fundamentales de equidad y responsabilidad. Esto subraya que la legitimidad del Estado de bienestar, en todas sus formas, depende intrínsecamente de la validación moral de los beneficiarios ante el público.

= Medición del merecimiento @renejcanales
<medición-del-merecimiento-falta-la-descripción-de-las-escalas-y-también-alternativas-de-operacionalización-renejcanales>
Como se esbozó anteriormente, la operacionalización empírica del merecimiento ha sido problemática, caracterizada por una desconexión entre la riqueza del concepto y la precisión de sus instrumentos de medición. La investigación en opinión pública tiende a tratar el merecimiento como una "caja negra", mas no necesariamente se estudiaba en profundidad @meuleman_welfare_2020. En este enfoque tradicional, las percepciones de merecimiento se inferían a partir de variables proxy, principalmente del apoyo a políticas sociales específicas o de la preferencia por el gasto público dirigido a ciertos grupos. Sin embargo, esta estrategia de inferencia indirecta presenta graves problemas de validez de constructo: el apoyo a una política no depende exclusivamente de la evaluación moral de los beneficiarios, sino que está influido ---como muestra la evidencia--- por consideraciones ideológicas sobre el tamaño del Estado, la eficiencia burocrática o el interés económico propio. Por lo tanto, medir el merecimiento a través del apoyo a políticas confunde el juicio moral sobre el "quién" con las preferencias políticas sobre el "cómo" @meuleman_welfare_2020.

Para superar estas limitaciones, la literatura reciente ha avanzado hacia estrategias de medición directa y de validación experimental. Un hito reciente es el desarrollo del módulo de encuesta "Hearts and Minds" por Epper y Mitrouchev @epper_measuring_2025, quienes demuestran que un conjunto parsimonioso de preguntas puede predecir el comportamiento real con la misma precisión que los costosos experimentos incentivados, permitiendo desagregar motivaciones que antes se confundían, como el altruismo y la aversión a la desigualdad.

Paralelamente, el uso de diseños experimentales sofisticados ha permitido aislar mecanismos causales: desde procedimientos de votación secuencial que distinguen entre principios de justicia y autointerés @chanel_preferences_2025, hasta la asignación aleatoria del éxito económico para demostrar cómo este moldea causalmente las creencias meritocráticas, resolviendo los problemas de endogeneidad y de asignación no aleatoria típicos de los estudios observacionales @fehr_misperceiving_2025.

== Medición directa mediante escalas psicométricas
<medición-directa-mediante-escalas-psicométricas>
Para superar estas limitaciones, la literatura reciente ha avanzado hacia la medición directa de los principios normativos mediante escalas psicométricas y módulos de encuesta validados. El avance más significativo en la medición de principios es la Escala de Principios de Merecimiento CARIN desarrollada por Meuleman, Roosma y Abts @meuleman_welfare_2020. A diferencia de los enfoques anteriores, este instrumento no pregunta por grupos específicos (como desempleados o inmigrantes), sino que busca captar la adhesión de los individuos a criterios abstractos de justicia (Control, Actitud, Reciprocidad, Identidad y Necesidad). Mediante modelos de ecuaciones estructurales, los autores demostraron que los cinco criterios no son intercambiables ni forman una única dimensión latente, sino que operan como factores distintos en la mente de los ciudadanos. Este enfoque permite distinguir empíricamente entre el "merecimiento condicional" (basado en el control y la reciprocidad) y el "merecimiento humanitario" (basado en la necesidad).

Recientemente, esta agenda metodológica se ha sofisticado aún más con el desarrollo del módulo "Hearts and Minds" por Epper y Mitrouchev @epper_measuring_2025 @renejcanales[no es de merecimiento este paper]. Estos autores han logrado validar un instrumento de encuesta parsimonioso que predice el comportamiento distributivo con la misma precisión que los experimentos económicos incentivados. Su contribución es fundamental porque permite desagregar dos motivaciones que las escalas tradicionales a menudo solapaban: la aversión a la desigualdad (un principio de justicia relacionado con la estructura de la distribución) y el altruismo (una preocupación por el bienestar del otro). Esto dota a los investigadores de herramientas escalables para evaluar no solo la estructura cognitiva de la solidaridad, sino también la intensidad de las preferencias sociales subyacentes, antes de que estas se apliquen al juicio de un grupo concreto.

== Diseños experimentales y encuestas factoriales @renejcanales[habla te demas redistributivos + generales]
<diseños-experimentales-y-encuestas-factoriales-acá-no-queda-claro-cómo-se-relaciona-con-la-medición-del-merecimiento-renejcanales-habla-te-demas-redistributivos-generales>
Paralelamente al desarrollo de escalas, una segunda corriente metodológica ha optado por el uso de diseños experimentales y encuestas factoriales para medir el merecimiento en acción. Mientras que las escalas capturan predisposiciones generales, los experimentos permiten aislar causalmente cómo características específicas de los actores evaluados y de los evaluadores activan o desactivan los juicios de merecimiento en contextos controlados.

En un estudio reciente, Gift y Lastra-Anadón @gift_deservingness_2023 utilizaron experimentos de encuesta para manipular la información sobre la "motivación" de los usuarios de servicios de salud, demostrando que incluso variaciones sutiles en la descripción del esfuerzo personal pueden alterar significativamente el apoyo a bienes públicos universales. De manera similar, Becker y Waitkus @becker_taxing_2025 y Sachweh y Eicher @sachweh_deserving_2023 aplican esta lógica al estudio de la riqueza, manipulando experimentalmente el origen de las fortunas (herencia vs.~emprendimiento; historia limpia vs.~historia ilegítima) para medir cómo dichos atributos inciden en la legitimidad percibida y en la demanda de impuestos.

Sin embargo, la frontera metodológica actual ha trascendido las situaciones hipotéticas para incorporar incentivos reales y diseños más complejos. Chanel et al. @chanel_preferences_2025 implementaron un experimento de "microdemocracia participativa" con votación secuencial incentivada, lo que les permitió observar no solo las preferencias declaradas, sino también las decisiones reales de redistribución, confirmando que los ciudadanos distinguen activamente entre "suerte bruta" y "suerte opcional" cuando hay dinero en juego . Por su parte, Fehr y Vollmann @fehr_misperceiving_2025 innovaron al manipular experimentalmente el éxito del propio participante mediante tareas de esfuerzo real, demostrando causalmente cómo la experiencia de ganar (incluso por suerte) transforma las creencias meritocráticas del individuo. Esta tendencia hacia la sofisticación se refleja también en grandes estudios poblacionales, como el de Kulic et al. @kulic_comprehensive_2025 en Italia, que integran múltiples tipos de experimentos (conjoint, tratamientos de información y vignettes) en una misma encuesta para capturar la multidimensionalidad de las percepciones de desigualdad .

Esta dualidad metodológica ---escalas versus experimentos situacionales e incentivados--- refleja dos caras complementarias del fenómeno. Las escalas, como la propuesta por Meuleman et al. @meuleman_welfare_2020, son fundamentales para entender el "mapa moral" estable de los individuos y cómo varía entre culturas o regímenes de bienestar. Por otro lado, los diseños experimentales son indispensables para capturar la elasticidad de estos juicios ante nueva información o cambios en el estatus personal, revelando cómo la exposición a datos sobre el bienestar o la experiencia propia de éxito puede reconfigurar, aunque sea momentáneamente, la aplicación de los criterios de merecimiento @mijs_belief_2022@fehr_misperceiving_2025. En conjunto, la literatura sugiere que una medición robusta debe trascender la simple pregunta de "quién merece qué", para interrogar sistemáticamente "bajo qué condiciones, incentivos y principios" se otorga esa legitimidad.

En conjunto, la literatura sugiere que una medición robusta del merecimiento debe trascender la simple pregunta de "quién merece qué", para interrogar sistemáticamente "bajo qué condiciones y principios" se otorga esa legitimidad.

= Conclusiones
<conclusiones>
El merecimiento emerge como un constructo multidimensional que estructura las percepciones ciudadanas sobre la justicia distributiva en el Estado de bienestar contemporáneo. Lejos de ser un juicio moral simple, las evaluaciones de merecimiento operan a través de múltiples dimensiones (Control, Actitud, Reciprocidad, Identidad y Necesidad) que pueden activarse de manera independiente o contradictoria según el contexto. La investigación más reciente ha permitido refinar este marco, demostrando que la ciudadanía aplica criterios de justicia sofisticados, como el "corte de responsabilidad", para distinguir entre la suerte bruta y la opcional @chanel_preferences_2025, y que es posible desagregar empíricamente si el apoyo redistributivo nace de la aversión a la desigualdad o del altruismo puro @epper_measuring_2025.

La expansión del concepto desde los "pobres no merecedores" hacia los "ricos no merecedores" representa un avance teórico crucial, pues reconoce que la legitimidad de la redistribución se juega en ambos extremos de la estructura social. Sin embargo, este juicio no es neutral: el éxito económico genera un sesgo de autojustificación que lleva a las élites a subestimar el papel de la suerte en sus logros, reforzando los ciclos de desigualdad mediante creencias meritocráticas distorsionadas @fehr_misperceiving_2025. Afortunadamente, los avances metodológicos ---desde escalas validadas hasta módulos de encuesta y experimentos de votación incentivada--- permiten ahora capturar estas dinámicas con una precisión inédita, superando las limitaciones históricas de la inferencia indirecta.

Finalmente, persisten desafíos importantes. La evidencia sobre la discrepancia entre la percepción y la realidad de las desigualdades @kulic_comprehensive_2025, sumada a la "ignorancia motivada" de los exitosos ante información correctiva @fehr_misperceiving_2025, sugiere que la formación de preferencias es un proceso cognitivamente sesgado. Además, la capacidad del merecimiento para penetrar incluso en los ámbitos de los derechos universales indica que la solidaridad ciudadana es más frágil y condicional de lo que se asume. Comprender estas dinámicas es esencial para diseñar Estados de bienestar que no solo sean eficientes, sino que también logren navegar las complejas intuiciones morales de una ciudadanía que exige, ante todo, justicia en los procesos.

 
  
#set bibliography(style: "../bib/apa6.csl") 


#bibliography("../bib/jusmer.bib")

