package org.kalypso.psiadapter.metadoc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;
import org.kalypso.psiadapter.PSICompactFactory;

import de.psi.go.lhwz.ECommException;

/**
 * PSICompactCommiter
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
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, java.util.Map, java.io.File)
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File docFile )
      throws MetaDocException
  {
    final File xmlFile = new File( FileUtilities.nameWithoutExtension( docFile.getAbsolutePath() ) + ".xml" );

    try
    {
      final Properties mdProps = new Properties();
      mdProps.putAll( metadata );

      final Writer writer = new OutputStreamWriter( new FileOutputStream( xmlFile ), "UTF-8" );
      // closes writer
      MetaDocSerializer.buildXML( serviceProps, mdProps, writer, docFile.getName() );

      // commit the both files (important: last one is the xml file)
      final String dist = serviceProps.getProperty( PSICOMPACT_DIST ) + "/";
      String distDocFile = dist + docFile.getName();
      distDocFile = distDocFile.replace( '\\', '/' );
      String distXmlFile = dist + xmlFile.getName();
      distXmlFile = distXmlFile.replace( '\\', '/' );

      distributeFile( docFile, distDocFile );
      distributeFile( xmlFile, distXmlFile );
    }
    catch( Exception e ) // generic for simplicity
    {
      throw new MetaDocException( e );
    }
    finally
    {
      docFile.delete();
      xmlFile.delete();
    }
  }

  private void distributeFile( final File file, final String distFile ) throws ECommException
  {
    m_logger.info( "Distributing File " + file.getAbsolutePath() + " to " + distFile );
    final boolean b = PSICompactFactory.getConnection().copyanddistributeFile( file, distFile );
    m_logger.info( "File distributed: " + b );
  }
}