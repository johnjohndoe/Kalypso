<?xml version="1.0" encoding="UTF-8"?>
<WspmProject xmlns:gml="http://www.opengis.net/gml" xmlns:swe="http://www.opengis.net/swe" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:common="org.kalypso.gml.common" xmlns:wspm="org.kalypso.model.wspm" xmlns:wspmClasses="org.kalypso.model.wspm.classifications" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:prof="org.kalypso.model.wspmprofile" xmlns:runoff="org.kalypso.model.wspmrunoff" xmlns:sweExt="org.kalypso.swe.ext" xmlns="org.kalypso.model.wspmproj" gml:id="root">
  <wspmClasses:classificationMember>
    <wspmClasses:WspmClassification gml:id="WspmClassification1">
      <gml:name>Beispielklassen</gml:name>
      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition1">
          <gml:description>Schlammsohle (Linie)</gml:description>
          <gml:name>mudHorizon</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style1A">
              <gml:description />
              <gml:name>Profillinie</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#FF9600</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style1B">
              <gml:description />
              <gml:name>Schlammsohle (Punkt)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1D">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1E">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1F">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#FF9600</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1G">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1H">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#FF9600</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter1I">
                  <wspmClasses:key>markerHeight</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition2">
          <gml:description>Bauwerkspunkte</gml:description>
          <gml:name>structureHorizon</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style2A">
              <gml:description />
              <gml:name>Bauwerkspunkte</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter2A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter2B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#0080B3</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter2C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition3">
          <gml:description>Unbekannte Punkte</gml:description>
          <gml:name>unknownPointsHorizon</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style3B">
              <gml:description />
              <gml:name>Unbekannte Punkte</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3D">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3E">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3F">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3G">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3H">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#000000</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3I">
                  <wspmClasses:key>markerHeight</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition8">
          <gml:description>Kreisprofil</gml:description>
          <gml:name>circleHorizon</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style8A">
              <gml:description />
              <gml:name>Durchlass</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter8A">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter8B">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter8C">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#FF00FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter8D">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>8</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter8E">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#000000</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter8F">
                  <wspmClasses:key>markerHeight</wspmClasses:key>
                  <wspmClasses:value>8</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>
    </wspmClasses:WspmClassification>
  </wspmClasses:classificationMember>
</WspmProject>
