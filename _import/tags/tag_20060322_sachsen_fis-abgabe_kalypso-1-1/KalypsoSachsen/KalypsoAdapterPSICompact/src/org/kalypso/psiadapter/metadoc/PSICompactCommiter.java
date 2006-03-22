package org.kalypso.psiadapter.metadoc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import org.apache.commons.configuration.Configuration;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;
import org.kalypso.psiadapter.PSICompactFactory;

import de.psi.go.lhwz.ECommException;

/**
 * PSICompactCommiter
 * 
 * TODO :PSICOMPACT_SUFFIX: wenn bei commiten noch vorhanden sollte durch eine default-value ersetzt werden (kann der
 * Fall sein wenn z.B. kein UserService zur Verfügung steht)
 * 
 * @author schlienger
 */
public class PSICompactCommiter implements IMetaDocCommiter
{
  /** distribution directory property */
  public final static String PSICOMPACT_DIST = "PSICOMPACT_DIST";

  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#prepareMetainf(java.util.Properties, java.util.Map)
   */
  public void prepareMetainf( final Properties serviceProps, final Map metadata )
  {
    final Properties props = new Properties();
    props.putAll( metadata );

    MetaDocSerializer.prepareProperties( serviceProps, props );

    metadata.putAll( props );
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, java.util.Map, java.io.File,
   *      java.lang.String, java.lang.String, org.apache.commons.configuration.Configuration)
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File docFile,
      final String identifier, final String category, final Configuration metadataExtensions ) throws MetaDocException
  {
    final String docFilePath = docFile.getAbsolutePath();
    final String goodDocFilePath = filenameCleaner( docFilePath );

    final File goodDocFile = new File( goodDocFilePath );
    docFile.renameTo( goodDocFile );

    final File xmlFile = new File( FileUtilities.nameWithoutExtension( goodDocFilePath ) + ".xml" );

    try
    {
      final Properties mdProps = new Properties();
      mdProps.putAll( metadata );

      final Writer writer = new OutputStreamWriter( new FileOutputStream( xmlFile ), "UTF-8" );
      // closes writer
      MetaDocSerializer.buildXML( serviceProps, mdProps, writer, goodDocFile.getName(), metadataExtensions );

      // commit the both files (important: last one is the xml file)
      final String dist = serviceProps.getProperty( PSICOMPACT_DIST ) + "/";

      final String distDocFile = dist + goodDocFile.getName();
      final String distXmlFile = dist + xmlFile.getName();

      distributeFile( goodDocFile, distDocFile );
      distributeFile( xmlFile, distXmlFile );
    }
    catch( final Exception e ) // generic for simplicity
    {
      throw new MetaDocException( e );
    }
    finally
    {
      docFile.delete();
      xmlFile.delete();
    }
  }

  /**
   * Dateinamen für PSICompact bereinigen.
   * <p>
   * Keine Umlaute, Spaces und die richtigen Slashes
   * </p>
   */
  private String filenameCleaner( final String filename )
  {
    String newName = filename;

    newName = newName.replace( '\\', '/' );
    newName = newName.replace( ' ', '_' );
    newName = newName.replaceAll( "ä", "ae" );
    newName = newName.replaceAll( "ö", "oe" );
    newName = newName.replaceAll( "ü", "ue" );
    newName = newName.replaceAll( "ß", "ss" );
    newName = newName.replaceAll( "Ä", "AE" );
    newName = newName.replaceAll( "Ö", "OE" );
    newName = newName.replaceAll( "Ü", "UE" );

    return newName;
  }

  private void distributeFile( final File file, final String distFile ) throws ECommException
  {
    m_logger.info( "Distributing File " + file.getAbsolutePath() + " to " + distFile );
    final boolean b = PSICompactFactory.getConnection().copyanddistributeFile( file, distFile );
    m_logger.info( "File distributed: " + b );
  }
}