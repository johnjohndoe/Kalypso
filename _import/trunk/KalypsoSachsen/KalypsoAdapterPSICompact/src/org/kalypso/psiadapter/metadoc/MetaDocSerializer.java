package org.kalypso.psiadapter.metadoc;

import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import org.kalypso.psiadapter.PSICompactFactory;

/**
 * MetaDocSerializer
 * 
 * @author schlienger
 */
public class MetaDocSerializer
{
  private final static String XML_HEADER = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";
  
  private final static String TAG_FOLTERDATA = "FolderData";
  private final static String TAG_HEADER = "";
  private final static String TAG_IMPORTMODE = "";
  private final static String TAG_RECORD = "";
  private final static String TAG_DOKUMENTTYP = "";
  private final static String TAG_ERSTELLER = "";
  private final static String TAG_AUTOR = "";
  private final static String TAG_REGION = "";
  private final static String TAG_EINGANGSDATUM = "";
  private final static String TAG_ERSTELLUNGSDATUM = "";
  private final static String TAG_ENDEGDATUM = "";
  private final static String TAG_VERSENDEN = "";
  private final static String TAG_APPFILES = "";

  /** date format for the date elements of the xml file */
  private final static DateFormat DF = new SimpleDateFormat( PSICompactFactory.getProperties().getProperty( PSICompactFactory.PROP_METADOC_DF, "yyyy-MM-ddTHH:mm:ss" ) );
  
  /**
   * Prepares the properties with some default value
   * 
   * TRICKY: we currently given the XSD-type of the value we expect
   * 
   * @param props
   */
  public static void prepareProperties( final Properties props )
  {
    props.setProperty( TAG_DOKUMENTTYP, "xs:string" );
    props.setProperty( TAG_ERSTELLER, "xs:string" );
    props.setProperty( TAG_AUTOR, "xs:string" );
    props.setProperty( TAG_ERSTELLUNGSDATUM, "xs:dateTime" );
    props.setProperty( TAG_ENDEGDATUM, "xs:dateTime" );
    props.setProperty( TAG_VERSENDEN, "xs:int" );
  }
  
  /**
   * Builds the XML using the given writer.
   * 
   * @param props
   * @param writer
   * @param fileName file name TODO: check if it is possible to have more that one file, and if yes, what is the separator char
   * @throws IOException
   */
  public static void buildXML( final Properties props, final Writer writer, final String fileName ) throws IOException
  {
    writer.write( XML_HEADER );
    writer.write( "<" + TAG_FOLTERDATA + ">" );
    writer.write( "<" + TAG_HEADER + ">" );
    writer.write( "<" + TAG_IMPORTMODE + ">" + 1 + "</" + TAG_IMPORTMODE + ">" );
    writer.write( "</" + TAG_HEADER + ">" );
    writer.write( "<" + TAG_RECORD + ">" );
    writer.write( "<" + TAG_DOKUMENTTYP + ">" + props.getProperty( TAG_DOKUMENTTYP, "" ) + "</" + TAG_DOKUMENTTYP + ">" );
    writer.write( "<" + TAG_ERSTELLER + ">" + props.getProperty( TAG_ERSTELLER, "" ) + "</" + TAG_ERSTELLER + ">" );
    writer.write( "<" + TAG_AUTOR + ">" + props.getProperty( TAG_AUTOR, "" ) + "</" + TAG_AUTOR + ">" );
    writer.write( "<" + TAG_REGION + ">" + props.getProperty( TAG_REGION, "ohne" ) + "</" + TAG_REGION + ">" );
    
    final String defaultDate = DF.format( new Date() );
    writer.write( "<" + TAG_EINGANGSDATUM + ">" + props.getProperty( TAG_EINGANGSDATUM, defaultDate ) + "</" + TAG_EINGANGSDATUM + ">" );
    writer.write( "<" + TAG_ERSTELLUNGSDATUM + ">" + props.getProperty( TAG_ERSTELLUNGSDATUM, defaultDate ) + "</" + TAG_ERSTELLUNGSDATUM + ">" );

    writer.write( "<" + TAG_ENDEGDATUM + ">" + props.getProperty( TAG_ENDEGDATUM, "" ) + "</" + TAG_ENDEGDATUM + ">" );
    writer.write( "<" + TAG_VERSENDEN + ">" + props.getProperty( TAG_VERSENDEN, "0" ) + "</" + TAG_VERSENDEN + ">" );

    writer.write( "<" + TAG_APPFILES + ">" + fileName + "</" + TAG_APPFILES + ">" );

    writer.write( "</" + TAG_RECORD + ">" );
    writer.write( "</" + TAG_FOLTERDATA + ">" );
  }
}
