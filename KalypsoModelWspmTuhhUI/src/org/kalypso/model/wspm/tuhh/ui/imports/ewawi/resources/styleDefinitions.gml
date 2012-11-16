<?xml version="1.0" encoding="UTF-8"?>
<WspmProject xmlns:gml="http://www.opengis.net/gml" xmlns:swe="http://www.opengis.net/swe" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:common="org.kalypso.gml.common" xmlns:wspm="org.kalypso.model.wspm" xmlns:wspmClasses="org.kalypso.model.wspm.classifications" xmlns:tuhh="org.kalypso.model.wspm.tuhh" xmlns:prof="org.kalypso.model.wspmprofile" xmlns:runoff="org.kalypso.model.wspmrunoff" xmlns:sweExt="org.kalypso.swe.ext" xmlns="org.kalypso.model.wspmproj" gml:id="root">
  <wspmClasses:classificationMember>
    <wspmClasses:WspmClassification gml:id="WspmClassification1">
      <gml:name>Beispielklassen</gml:name>
      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition1">
          <gml:description>Gewässersohle</gml:description>
          <gml:name>EWAWI_0</gml:name>
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
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition2">
          <gml:description>Erster unterströmter Horizont</gml:description>
          <gml:name>EWAWI_1</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style2A">
              <gml:description />
              <gml:name>Bauwerksunterkante (Linie)</gml:name>
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
          <gml:description>Erster überströmter Horizont</gml:description>
          <gml:name>EWAWI_2</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style3A">
              <gml:description />
              <gml:name>Generisch (Linie)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter3C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style3B">
              <gml:description />
              <gml:name>Generisch (Punkt)</gml:name>
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
        <wspmClasses:StyleDefinition gml:id="StyleDefinition4">
          <gml:description>Zweiter unterströmter Horizont</gml:description>
          <gml:name>EWAWI_3</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style4A">
              <gml:description />
              <gml:name>Generisch (Linie)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style4B">
              <gml:description />
              <gml:name>Generisch (Punkt)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4D">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4E">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4F">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4G">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4H">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#000000</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter4I">
                  <wspmClasses:key>markerHeight</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition5">
          <gml:description>Zweiter überströmter Horizont</gml:description>
          <gml:name>EWAWI_4</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style5A">
              <gml:description />
              <gml:name>Generisch (Linie)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style5B">
              <gml:description />
              <gml:name>Generisch (Punkt)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5D">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5E">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5F">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5G">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5H">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#000000</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter5I">
                  <wspmClasses:key>markerHeight</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition6">
          <gml:description>Dritter unterströmter Horizont</gml:description>
          <gml:name>EWAWI_5</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style6A">
              <gml:description />
              <gml:name>Generisch (Linie)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style6B">
              <gml:description />
              <gml:name>Generisch (Punkt)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6D">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6E">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6F">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6G">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6H">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#000000</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter6I">
                  <wspmClasses:key>markerHeight</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
        </wspmClasses:StyleDefinition>
      </wspmClasses:styleDefinition>

      <wspmClasses:styleDefinition>
        <wspmClasses:StyleDefinition gml:id="StyleDefinition7">
          <gml:description>Dritter überströmter Horizont</gml:description>
          <gml:name>EWAWI_6</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style7A">
              <gml:description />
              <gml:name>Generisch (Linie)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7A">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>2</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7B">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7C">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>line</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
            </wspmClasses:Style>
          </wspmClasses:styleMember>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style7B">
              <gml:description />
              <gml:name>Generisch (Punkt)</gml:name>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7D">
                  <wspmClasses:key>type</wspmClasses:key>
                  <wspmClasses:value>point</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7E">
                  <wspmClasses:key>strokeWidth</wspmClasses:key>
                  <wspmClasses:value>1</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7F">
                  <wspmClasses:key>fillColor</wspmClasses:key>
                  <wspmClasses:value>#9900FF</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7G">
                  <wspmClasses:key>markerWidth</wspmClasses:key>
                  <wspmClasses:value>5</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7H">
                  <wspmClasses:key>strokeColor</wspmClasses:key>
                  <wspmClasses:value>#000000</wspmClasses:value>
                </wspmClasses:StyleParameter>
              </wspmClasses:parameterMember>
              <wspmClasses:parameterMember>
                <wspmClasses:StyleParameter gml:id="StyleParameter7I">
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
          <gml:name>EWAWI_7</gml:name>
          <wspmClasses:styleMember>
            <wspmClasses:Style gml:id="Style8A">
              <gml:description />
              <gml:name>Durchlass (Punkt)</gml:name>
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
