<?xml version="1.0" encoding="UTF-8" ?>
<gml:Dictionary xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="
     http://www.opengis.net/gml http://dev.bjoernsen.de/ogc/schema/gml/3.1.1/base/gml.xsd"
     xmlns:gml="http://www.opengis.net/gml" gml:id="uom">

     <gml:name>Einheiten in KALYPSO</gml:name>

        <!-- das klappt -->
     <gml:dictionaryEntry>
          <gml:BaseUnit gml:id="cmaP">
               <gml:name>Centimeter am Pegel</gml:name>
               <gml:quantityType>Länge</gml:quantityType>
               <gml:unitsSystem/>
          </gml:BaseUnit>
     </gml:dictionaryEntry>

        <!-- das klappt noch nicht -->
     <gml:dictionaryEntry>
          <gml:ConventionalUnit gml:id="mm">
               <gml:name>Millimeter</gml:name>
               <gml:quantityType>Laenge</gml:quantityType>
               <gml:conversionToPreferredUnit uom="#m">
                    <gml:factor>1000</gml:factor>
               </gml:conversionToPreferredUnit>
          </gml:ConventionalUnit>
     </gml:dictionaryEntry>
</gml:Dictionary>