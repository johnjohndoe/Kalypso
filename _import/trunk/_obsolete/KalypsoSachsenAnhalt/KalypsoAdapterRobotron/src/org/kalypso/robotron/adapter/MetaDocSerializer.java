/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.robotron.adapter;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.hwv.services.metadoc.IDocumentServiceConstants;
import org.kalypso.robotron.UrlCatalogRobotron;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Holger Albert
 */
public class MetaDocSerializer
{
  private static String XML_HEADER = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";

  private static String LT = "<";

  private static String CLT = "</";

  private static String GT = ">";

  private static String TAG_META = "meta";

  private static String TAG_AUTOR = "autor";

  private static String TAG_DESCRIPTION = "description";

  private static String TAG_ERSTELLER = "ersteller";

  private static String TAG_ERSTELLUNGSDATUM = "erstelldat";

  private static String TAG_GUELTIGKEITSDATUM = "gueltigdat";

  private static String TAG_DOKUMENT = "dokument";

  private static String TAG_DOKUMENTTYP = "doktyp";

  private static String TAG_GEBIET = "gebiet";

  private static String TAG_STATION = "station_id";

  private static String TAG_SIMULATION = "simulation";

  private static String TAG_SCHLUESSEL = "schluessel";

  private static String TAG_TYP = "typ";

  private static String TAG_KLASSE = "klasse";

  private static String TAG_FILES = "files";

  private static String TAG_FILE = "file";

  private static DateFormat DFDATETIME = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

  private static QName QNAME_PROP_AUTOR = new QName( UrlCatalogRobotron.NS_METADOC, TAG_AUTOR );

  private static QName QNAME_PROP_ERSTELLER = new QName( UrlCatalogRobotron.NS_METADOC, TAG_ERSTELLER );

  private static QName QNAME_PROP_ERSTELLUNGSDATUM = new QName( UrlCatalogRobotron.NS_METADOC, TAG_ERSTELLUNGSDATUM );

  private static QName QNAME_PROP_GUELTIGKEITSDATUM = new QName( UrlCatalogRobotron.NS_METADOC, TAG_GUELTIGKEITSDATUM );

  private static QName QNAME_PROP_DOKUMENTTYP = new QName( UrlCatalogRobotron.NS_METADOC, TAG_DOKUMENTTYP );

  private static QName QNAME_PROP_DOKUMENT = new QName( UrlCatalogRobotron.NS_METADOC, TAG_DOKUMENT );

  private static QName QNAME_PROP_DESCRIPTION = new QName( UrlCatalogRobotron.NS_METADOC, TAG_DESCRIPTION );

  private static QName QNAME_PROP_SCENARIO = new QName( UrlCatalogRobotron.NS_METADOC, "scenarioId" );

  private static QName QNAME_PROP_GEBIET = new QName( UrlCatalogRobotron.NS_METADOC, TAG_GEBIET );

  /**
   * The constructor.
   */
  private MetaDocSerializer( )
  {
  }

  /**
   * This function builds the metadata XML.
   */
  public static String buildXML( String fileName, Feature metadataFeature )
  {
    StringBuffer bf = new StringBuffer();

    bf.append( XML_HEADER );
    bf.append( LT + TAG_META + GT );

    String autor = (String) metadataFeature.getProperty( QNAME_PROP_AUTOR );
    String ersteller = (String) metadataFeature.getProperty( QNAME_PROP_ERSTELLER );
    XMLGregorianCalendar erstellungsDatum = (XMLGregorianCalendar) metadataFeature.getProperty( QNAME_PROP_ERSTELLUNGSDATUM );
    XMLGregorianCalendar gueltigkeitsDatum = (XMLGregorianCalendar) metadataFeature.getProperty( QNAME_PROP_GUELTIGKEITSDATUM );
    String dokumentTyp = (String) metadataFeature.getProperty( QNAME_PROP_DOKUMENTTYP );
    String dokument = (String) metadataFeature.getProperty( QNAME_PROP_DOKUMENT );
    String identifier = (String) metadataFeature.getProperty( IDocumentServiceConstants.QNAME_META_DOCUMENT_ID );
    String category = (String) metadataFeature.getProperty( IDocumentServiceConstants.QNAME_META_DOCUMENT_CATEGORY );
    String description = (String) metadataFeature.getProperty( QNAME_PROP_DESCRIPTION );
    String scenarioId = (String) metadataFeature.getProperty( QNAME_PROP_SCENARIO );
    if( scenarioId == null )
      scenarioId = "";
    String simulation = scenarioId.length() == 0 ? "0" : "1";
    String gebiet = (String) metadataFeature.getProperty( QNAME_PROP_GEBIET );
    String stationId = (String) metadataFeature.getProperty( IDocumentServiceConstants.QNAME_META_STATION_ID );
    if( stationId == null )
      stationId = "";

    bf.append( LT + TAG_AUTOR + GT + autor + CLT + TAG_AUTOR + GT );
    bf.append( LT + TAG_ERSTELLER + GT + ersteller + CLT + TAG_ERSTELLER + GT );
    bf.append( LT + TAG_ERSTELLUNGSDATUM + GT + formatDate( erstellungsDatum ) + CLT + TAG_ERSTELLUNGSDATUM + GT );
    bf.append( LT + TAG_GUELTIGKEITSDATUM + GT + formatDate( gueltigkeitsDatum ) + CLT + TAG_GUELTIGKEITSDATUM + GT );
    bf.append( LT + TAG_DOKUMENTTYP + GT + dokumentTyp + CLT + TAG_DOKUMENTTYP + GT );
    bf.append( LT + TAG_DOKUMENT + GT + dokument + CLT + TAG_DOKUMENT + GT );
    bf.append( LT + TAG_SCHLUESSEL + GT + identifier + CLT + TAG_SCHLUESSEL + GT );

    String klasse = "";
    String typ = "";
    if( category != null )
    {
      String[] splits = category.split( ";", 2 );
      if( splits.length == 2 )
      {
        typ = splits[0];
        klasse = splits[1];
      }
      else if( splits.length == 1 )
        klasse = splits[0];
    }

    bf.append( LT + TAG_TYP + GT + typ + CLT + TAG_TYP + GT );
    bf.append( LT + TAG_KLASSE + GT + klasse + CLT + TAG_KLASSE + GT );
    bf.append( LT + TAG_DESCRIPTION + GT + description + CLT + TAG_DESCRIPTION + GT );
    bf.append( LT + TAG_SIMULATION + GT + simulation + CLT + TAG_SIMULATION + GT );
    bf.append( LT + TAG_GEBIET + GT + gebiet + CLT + TAG_GEBIET + GT );

    /* Robotron can only requests one station_id, otherwise it would make no sense. */
    String[] stationIDs = stationId.split( ";" );
    if( stationIDs != null && stationIDs.length == 1 )
      bf.append( LT + TAG_STATION + GT + stationIDs[0] + CLT + TAG_STATION + GT );
    else
      bf.append( LT + TAG_STATION + GT + "" + CLT + TAG_STATION + GT );

    bf.append( LT + TAG_FILES + GT );
    bf.append( LT + TAG_FILE + GT + fileName + CLT + TAG_FILE + GT );
    bf.append( CLT + TAG_FILES + GT );

    bf.append( CLT + TAG_META + GT );

    return bf.toString();
  }

  /**
   * This function formats the given calendar.
   * 
   * @param cal
   *          The xml calendar.
   * @return A formatted date string.
   */
  private static String formatDate( XMLGregorianCalendar cal )
  {
    Date date = DateUtilities.toDate( cal );
    if( date == null )
      return "";

    return DFDATETIME.format( date );
  }
}