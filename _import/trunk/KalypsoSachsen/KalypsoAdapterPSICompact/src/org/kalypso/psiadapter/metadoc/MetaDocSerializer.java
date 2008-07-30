package org.kalypso.psiadapter.metadoc;

import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.psiadapter.UrlCatalogPSI;
import org.kalypsodeegree.model.feature.Feature;

/**
 * MetaDocSerializer Changes:
 * <ul>
 * <li>Changed Tagname for LHVZINTERN from "LHWZ-intern" to "LHWZIntern" (email from Büttner, 04.05.2005)
 * </ul>
 * 
 * @author schlienger
 */
public class MetaDocSerializer
{
  private final static String XML_HEADER = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";

  private final static String TAG_FOLDERDATA = "FolderData";

  private final static String TAG_HEADER = "Header";

  public final static String TAG_IMPORTMODE = "ImportMode";

  private final static String TAG_RECORD = "Record";

  private final static String TAG_DOKUMENTTYP = "Dokumenttyp";

  private final static String TAG_ERSTELLER = "Ersteller";

  private final static String TAG_AUTOR = "Autor";

  private final static String TAG_REGION = "Region";

  private final static String TAG_EINGANGSDATUM = "Eingangsdatum";

  private final static String TAG_ERSTELLUNGSDATUM = "Erstellungsdatum";

  private final static String TAG_ENDEGDATUM = "EndeGDatum";

  public final static String TAG_VERSENDEN = "Versenden";

  private static final String TAG_LHWZINTERN = "LHWZIntern";

  private final static String TAG_APPFILES = "AppFiles";

  private final static DateFormat DFDATETIME = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

  private final static DateFormat DFDATE = new SimpleDateFormat( "yyyy-MM-dd" );

  private static final QName QNAME_PROP_SCENARIO = new QName( UrlCatalogPSI.NS_METADOC, "scenarioId" );

  private static final QName QNAME_PROP_REGION = new QName( UrlCatalogPSI.NS_METADOC, TAG_REGION );

  private static final QName QNAME_PROP_ERSTELLER = new QName( UrlCatalogPSI.NS_METADOC, TAG_ERSTELLER );

  private static final QName QNAME_PROP_AUTOR = new QName( UrlCatalogPSI.NS_METADOC, TAG_AUTOR );

  private static final QName QNAME_PROP_ERSTELLUNGSDATUM = new QName( UrlCatalogPSI.NS_METADOC, TAG_ERSTELLUNGSDATUM );

  private static final QName QNAME_PROP_ENDGDATUM = new QName( UrlCatalogPSI.NS_METADOC, TAG_ENDEGDATUM );

  /**
   * Builds the XML using the given writer.
   * 
   * @param serviceProps
   *          properties of the service
   * @param mdProps
   *          properties of the metadata
   * @param metadataExtensions
   */
  public static void buildXML( final Writer writer, final String fileName, final Feature metadataFeature, final String importMode, final String versenden ) throws IOException
  {
    writer.write( XML_HEADER );
    writer.write( "<" + TAG_FOLDERDATA + ">" );
    writer.write( "<" + TAG_HEADER + ">" );
    writer.write( "<" + TAG_IMPORTMODE + ">" + importMode + "</" + TAG_IMPORTMODE + ">" );
    writer.write( "</" + TAG_HEADER + ">" );
    writer.write( "<" + TAG_RECORD + ">" );

    // retrieve scenario id
    final String scenarioId = (String) metadataFeature.getProperty( QNAME_PROP_SCENARIO );
    final String region = (String) metadataFeature.getProperty( QNAME_PROP_REGION );
    final String ersteller = (String) metadataFeature.getProperty( QNAME_PROP_ERSTELLER );
    final String autor = (String) metadataFeature.getProperty( QNAME_PROP_AUTOR );
    final XMLGregorianCalendar erstellungsDatum = (XMLGregorianCalendar) metadataFeature.getProperty( QNAME_PROP_ERSTELLUNGSDATUM );
    final XMLGregorianCalendar endgDatum = (XMLGregorianCalendar) metadataFeature.getProperty( QNAME_PROP_ENDGDATUM );
    final String dokTyp = scenarioId.length() == 0 ? "Prognose" : "Prognose_" + scenarioId;

    final String erstellungsString = formatDate( erstellungsDatum );
    final String endgString = formatDate( endgDatum );
    
    writer.write( "<" + TAG_DOKUMENTTYP + ">" + dokTyp + "</" + TAG_DOKUMENTTYP + ">" );
    writer.write( "<" + TAG_ERSTELLER + ">" + ersteller + "</" + TAG_ERSTELLER + ">" );
    writer.write( "<" + TAG_AUTOR + ">" + autor + "</" + TAG_AUTOR + ">" );
    writer.write( "<" + TAG_REGION + ">" + region + "</" + TAG_REGION + ">" );

    writer.write( "<" + TAG_EINGANGSDATUM + ">" + DFDATETIME.format( new Date() ) + "</" + TAG_EINGANGSDATUM + ">" );
    writer.write( "<" + TAG_ERSTELLUNGSDATUM + ">" + erstellungsString + "</" + TAG_ERSTELLUNGSDATUM + ">" );

    writer.write( "<" + TAG_ENDEGDATUM + ">" + endgString + "</" + TAG_ENDEGDATUM + ">" );
    writer.write( "<" + TAG_VERSENDEN + ">" + versenden + "</" + TAG_VERSENDEN + ">" );

    writer.write( "<" + TAG_LHWZINTERN + ">1</" + TAG_LHWZINTERN + ">" );

    writer.write( "<" + TAG_APPFILES + ">" + fileName + "</" + TAG_APPFILES + ">" );

    writer.write( "</" + TAG_RECORD + ">" );
    writer.write( "</" + TAG_FOLDERDATA + ">" );
  }

  private static String formatDate( XMLGregorianCalendar cal )
  {
    Date date = DateUtilities.toDate( cal );
    if( date == null )
      return "";
    
    return DFDATE.format( date );
  }
}