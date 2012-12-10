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
package org.kalypso.model.hydrology.internal.postprocessing;

import org.apache.commons.lang.StringUtils;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class DefaultPathGenerator
{
  public static String generateResultPathFor( final Feature feature, final String suffix, final String extra )
  {
    final String extraString;
    if( extra == null )
      extraString = ""; //$NON-NLS-1$
    else
      extraString = extra;
    final String observationTitle = getObservationTitle( feature );

    final String annotationName = getAnnotationName( feature );
    final String result = annotationName + "/" + observationTitle + extraString + "/" + getTitleForSuffix( suffix ) + ".zml"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    return result;
  }

  private static String getAnnotationName( final Feature feature )
  {
    final IAnnotation annotation = feature.getFeatureType().getAnnotation();
    return annotation.getValue( IAnnotation.ANNO_NAME );
  }

  private static String getObservationTitle( final Feature feature )
  {
    final String feName = feature.getName();
    if( StringUtils.isBlank( feName ) )
      return feature.getId();

    return feName;
  }

  public static String generateTitleForObservation( final Feature feature, final String suffix )
  {
    final String observationTitle = getObservationTitle( feature );
    final String annotationName = getAnnotationName( feature );
    return observationTitle + " - " + DefaultPathGenerator.getTitleForSuffix( suffix ) + " " + annotationName; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static String getTitleForSuffix( final String suffix )
  {
    // j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.0");  //$NON-NLS-1$
    // j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.1");  //$NON-NLS-1$
    // n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.2");  //$NON-NLS-1$
    // j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.3");  //$NON-NLS-1$
    // n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.4");  //$NON-NLS-1$
    // n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.5");  //$NON-NLS-1$
    // j Gesamtabfluss Knoten .qgs
    if( suffix.equalsIgnoreCase( "qgs" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.6");  //$NON-NLS-1$
    // n Gesamtabfluss TG .qgg
    if( suffix.equalsIgnoreCase( "qgg" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.7");  //$NON-NLS-1$
    // n Oberflaechenabfluss .qna
    if( suffix.equalsIgnoreCase( "qna" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.8");  //$NON-NLS-1$
    // n Interflow .qif
    if( suffix.equalsIgnoreCase( "qif" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.9");  //$NON-NLS-1$
    // n Abfluss vers. Flaechen .qvs
    if( suffix.equalsIgnoreCase( "qvs" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.10");  //$NON-NLS-1$
    // n Basisabfluss .qbs
    if( suffix.equalsIgnoreCase( "qbs" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.11");  //$NON-NLS-1$
    // n Kluftgrundw1 .qt1
    if( suffix.equalsIgnoreCase( "qt1" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.12");  //$NON-NLS-1$
    // n Kluftgrundw .qtg
    if( suffix.equalsIgnoreCase( "qtg" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.13");  //$NON-NLS-1$
    // n Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgw" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.14");  //$NON-NLS-1$
    // n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.15");  //$NON-NLS-1$
    if( suffix.equalsIgnoreCase( "qmr" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.16");  //$NON-NLS-1$
    // n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.17");  //$NON-NLS-1$
    // n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.18");  //$NON-NLS-1$
    // n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.19");  //$NON-NLS-1$
    // n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.20");  //$NON-NLS-1$
    // n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.21");  //$NON-NLS-1$
    // n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.22");  //$NON-NLS-1$
    // n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.23");  //$NON-NLS-1$
    // n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.24");  //$NON-NLS-1$
    // n Speicherueberlauf .sub
    if( suffix.equalsIgnoreCase( "sub" ) )  //$NON-NLS-1$
      return Messages.getString("org.kalypso.convert.namodel.DefaultPathGenerator.25");  //$NON-NLS-1$
    // n Kapil.Aufstieg/Perkolation .kap - not available, because not used in the calculation core
    // if( suffix.equalsIgnoreCase( "kap" ) )
    // return "Kapil.Aufstieg/Perkolation";
    return suffix;
  }

}
