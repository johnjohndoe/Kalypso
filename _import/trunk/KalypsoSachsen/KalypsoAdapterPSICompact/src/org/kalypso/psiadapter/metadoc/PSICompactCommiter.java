package org.kalypso.psiadapter.metadoc;

import java.io.File;
import java.io.FileWriter;
import java.util.Properties;
import java.util.logging.Logger;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.MetaDocException;
import org.kalypso.metadoc.beans.DocBean;
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
   * @see org.kalypso.metadoc.IMetaDocCommiter#prepareMetainf(java.util.Properties, org.kalypso.metadoc.beans.DocBean)
   */
  public void prepareMetainf( final Properties serviceProps, final DocBean docBean )
  {
    final Properties props = new Properties();
    props.putAll( docBean.getMetadata() );

    MetaDocSerializer.prepareProperties( serviceProps, props );

    docBean.getMetadata().putAll( props );
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, org.kalypso.metadoc.beans.DocBean)
   */
  public void commitDocument( final Properties serviceProps, final DocBean docBean ) throws MetaDocException
  {
    final File xmlFile = new File( FileUtilities.nameWithoutExtension( docBean
        .getLocation() )
        + ".xml" );

    // just fetch the name of the file (without path, not necessary for PSICompact)
    final File docFile = new File( docBean.getLocation() );
    
    try
    {
      final Properties mdProps = new Properties();
      mdProps.putAll( docBean.getMetadata() );
      
      final FileWriter writer = new FileWriter( xmlFile );
      // closes writer 
      MetaDocSerializer.buildXML( serviceProps, mdProps, writer, docFile.getName() );
      
      // commit the both files (important: last one is the xml file)
      final String dist = serviceProps.getProperty( PSICOMPACT_DIST ) + "/";
      String distDocFile = dist + docFile.getName();
      distDocFile = distDocFile.replace( '\\', '/' );
      String distXmlFile = dist + xmlFile.getName();
      distXmlFile = distXmlFile.replace( '\\', '/' );

      // todo: das doc wird gleich nach dieser Operation gel�scht
      // ist das ok? wenn ja, bitte kommentar hier einf�gen
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