/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.convert.namodel;

import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class DefaultPathGenerator
{

  public static String generateResultPathFor( final Feature feature, final String titleProperty, final String suffix, final String extra )
  {
    final String extraString;
    if( extra == null )
      extraString = "";
    else
      extraString = extra;
    final String observationTitle = getObservationTitle( feature, titleProperty );
    
    final String annotationName = getAnnotationName( feature );
    final String result = "Berechnet/" + annotationName + "/" + observationTitle + extraString + "/" + getTitleForSuffix( suffix ) + ".zml";
//    return result.replaceAll( "Node", "Knoten" );
    return result;
  }

  private static String getAnnotationName( final Feature feature )
  {
    final IAnnotation annotation = AnnotationUtilities.getAnnotation( feature.getFeatureType() );
    return annotation != null ? annotation.getValue(IAnnotation.ANNO_NAME) : feature.getFeatureType().getQName().getLocalPart();
  }

  private static String getObservationTitle( final Feature feature, final String titleProperty )
  {
    final String feName = (String) feature.getProperty( titleProperty );
    if( feName != null && feName.length() > 0 )
      return feName;
    else
      return feature.getId();
  }

  public static String generateTitleForObservation( final Feature feature, final String titleProperty, final String suffix )
  {
    final String observationTitle = getObservationTitle( feature, titleProperty );
    final String annotationName = getAnnotationName( feature );
    return observationTitle + " - " + DefaultPathGenerator.getTitleForSuffix( suffix ) + " " + annotationName;
  }

  public static String getTitleForSuffix( String suffix )
  {
    // j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )
      return "Temperatur";
    // j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )
      return "Niederschlag";
    // n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )
      return "Schneehoehe";
    // j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )
      return "Bodenfeuchte";
    // n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )
      return "Bodenspeicherbilanz";
    // n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )
      return "Grundwasserstand";
    // j Gesamtabfluss Knoten .qgs
    if( suffix.equalsIgnoreCase( "qgs" ) )
      return "Gesamtabfluss";
    // n Gesamtabfluss TG .qgg
    if( suffix.equalsIgnoreCase( "qgg" ) )
      return "Gesamtabfluss";
    // n Oberflaechenabfluss .qna
    if( suffix.equalsIgnoreCase( "qna" ) )
      return "Oberflaechenabfluss(natuerlich)";
    // n Interflow .qif
    if( suffix.equalsIgnoreCase( "qif" ) )
      return "Interflow";
    // n Abfluss vers. Flaechen .qvs
    if( suffix.equalsIgnoreCase( "qvs" ) )
      return "Oberflaechenabfluss(versiegelt)";
    // n Basisabfluss .qbs
    if( suffix.equalsIgnoreCase( "qbs" ) )
      return "Basisabfluss";
    // n Kluftgrundw1 .qt1
    if( suffix.equalsIgnoreCase( "qt1" ) )
      return "Kluftgrundw1abfluss";
    // n Kluftgrundw .qtg
    if( suffix.equalsIgnoreCase( "qtg" ) )
      return "KluftGWAbfluss";
    // n Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgw" ) )
      return "Grundwasserabfluss";
    // n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )
      return "Evapotranspiration";
    if( suffix.equalsIgnoreCase( "qmr"))
      return "MuldenRigolen";
    // n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )
      return "Hydrotope";
    // n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )
      return "Abflussbilanz";
    // n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )
      return "Abflusswerte(statistisch)";
    // n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )
      return "Fuellvolumen";
    // n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) )
      return "Wasserspiegelhoehe";
    // n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) )
      return "Talsperrenverdunstung";
    // n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) )
      return "Niederschlag";
    // n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) )
      return "Zehrung";
    // n Speicherueberlauf .sub
    if( suffix.equalsIgnoreCase( "sub" ) )
      return "Speicherueberlauf";
    // n Kapil.Aufstieg/Perkolation .kap - not available, because not used in the calculation core
    // if( suffix.equalsIgnoreCase( "kap" ) )
    // return "Kapil.Aufstieg/Perkolation";
    return suffix;
  }

}
