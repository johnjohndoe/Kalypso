package org.kalypso.psiadapter.metadoc;

import java.io.IOException;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Properties;

import org.apache.commons.io.IOUtils;

/**
 * MetaDocSerializer
 * 
 * @author schlienger
 */
public class MetaDocSerializer
{
  private final static String XML_HEADER = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";

  private final static String TAG_FOLDERDATA = "FolderData";

  private final static String TAG_HEADER = "Header";

  private final static String TAG_IMPORTMODE = "ImportMode";

  private final static String TAG_RECORD = "Record";

  private final static String TAG_DOKUMENTTYP = "Dokumenttyp";

  private final static String TAG_ERSTELLER = "Ersteller";

  private final static String TAG_AUTOR = "Autor";

  private final static String TAG_REGION = "Region";

  private final static String TAG_EINGANGSDATUM = "Eingangsdatum";

  private final static String TAG_ERSTELLUNGSDATUM = "Erstellungsdatum";

  private final static String TAG_ENDEGDATUM = "EndeGDatum";

  private final static String TAG_VERSENDEN = "Versenden";

  private final static String TAG_APPFILES = "AppFiles";

  /** date format for the date elements of the xml file */
  private final static DateFormat DF = new SimpleDateFormat(
      "yyyy-MM-dd'T'HH:mm:ss" );

  /**
   * Prepares the properties with some default value
   * 
   * TRICKY: we currently given the XSD-type of the value we expect and the
   * default value. The value stuff is a ';' separated string
   * 
   * @param serviceProps
   * @param props
   */
  public static void prepareProperties( final Properties serviceProps,
      final Properties props )
  {
    props.setProperty( TAG_DOKUMENTTYP, "string;"
        + serviceProps.getProperty( TAG_DOKUMENTTYP, "Dokumenttyp" ) );
    props.setProperty( TAG_REGION, "string;" + serviceProps.getProperty( TAG_REGION, "Region" ) );
    props.setProperty( TAG_ERSTELLER, "string;"
        + serviceProps.getProperty( TAG_ERSTELLER, "Ersteller" ) );
    props.setProperty( TAG_AUTOR, "string;Autor" );
    props.setProperty( TAG_ERSTELLUNGSDATUM, "dateTime" );
    props.setProperty( TAG_ENDEGDATUM, "dateTime" );
  }

  /**
   * Builds the XML using the given writer.
   * 
   * @param serviceProps
   *          properties of the service
   * @param mdProps
   *          properties of the metadata
   * @param writer
   * @param fileName
   * @throws IOException
   */
  public static void buildXML( final Properties serviceProps,
      final Properties mdProps, final Writer writer, final String fileName ) throws IOException
  {
    try
    {
      writer.write( XML_HEADER );
      writer.write( "<" + TAG_FOLDERDATA + ">" );
      writer.write( "<" + TAG_HEADER + ">" );
      writer.write( "<" + TAG_IMPORTMODE + ">"
          + serviceProps.getProperty( TAG_IMPORTMODE, "1" ) + "</"
          + TAG_IMPORTMODE + ">" );
      writer.write( "</" + TAG_HEADER + ">" );
      writer.write( "<" + TAG_RECORD + ">" );
      writer.write( "<" + TAG_DOKUMENTTYP + ">"
          + mdProps.getProperty( TAG_DOKUMENTTYP, "" ) + "</" + TAG_DOKUMENTTYP
          + ">" );
      writer.write( "<" + TAG_ERSTELLER + ">"
          + mdProps.getProperty( TAG_ERSTELLER, "" ) + "</" + TAG_ERSTELLER
          + ">" );
      writer.write( "<" + TAG_AUTOR + ">" + mdProps.getProperty( TAG_AUTOR, "" )
          + "</" + TAG_AUTOR + ">" );
      writer
          .write( "<" + TAG_REGION + ">"
              + mdProps.getProperty( TAG_REGION, "ohne" ) + "</" + TAG_REGION
              + ">" );

      final String defaultDate = DF.format( new Date() );
      writer.write( "<" + TAG_EINGANGSDATUM + ">"
          + mdProps.getProperty( TAG_EINGANGSDATUM, defaultDate ) + "</"
          + TAG_EINGANGSDATUM + ">" );
      writer.write( "<" + TAG_ERSTELLUNGSDATUM + ">"
          + mdProps.getProperty( TAG_ERSTELLUNGSDATUM, defaultDate ) + "</"
          + TAG_ERSTELLUNGSDATUM + ">" );

      writer.write( "<" + TAG_ENDEGDATUM + ">"
          + mdProps.getProperty( TAG_ENDEGDATUM, "" ) + "</" + TAG_ENDEGDATUM
          + ">" );
      writer.write( "<" + TAG_VERSENDEN + ">"
          + serviceProps.getProperty( TAG_VERSENDEN, "0" ) + "</"
          + TAG_VERSENDEN + ">" );

      writer.write( "<" + TAG_APPFILES + ">" + fileName + "</" + TAG_APPFILES
          + ">" );

      writer.write( "</" + TAG_RECORD + ">" );
      writer.write( "</" + TAG_FOLDERDATA + ">" );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}