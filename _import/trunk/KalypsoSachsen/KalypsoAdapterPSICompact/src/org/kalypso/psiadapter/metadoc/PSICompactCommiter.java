package org.kalypso.psiadapter.metadoc;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.eclipse.osgi.framework.internal.core.FrameworkProperties;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.hwv.services.metadoc.DocumentServiceSimulation;
import org.kalypso.hwv.services.metadoc.IDocumentServiceConstants;
import org.kalypso.psiadapter.PSICompactFactory;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypsodeegree.model.feature.Feature;

import de.psi.go.lhwz.ECommException;

/**
 * PSICompactCommiter TODO :PSICOMPACT_SUFFIX: wenn bei commiten noch vorhanden sollte durch eine default-value ersetzt
 * werden (kann der Fall sein wenn z.B. kein UserService zur Verfügung steht)
 * 
 * @author Marc Schlienger
 * @author Gernot Belger
 * @author Holger Albert
 */
public class PSICompactCommiter extends DocumentServiceSimulation
{
  /** distribution directory property */
  public final static String SYSPROP_PSICOMPACT_DIST = IDocumentServiceConstants.SYSPROP_BASE + ".psidistdir";

  public final static String SYSPROP_IMPORTMODE = IDocumentServiceConstants.SYSPROP_BASE + "." + MetaDocSerializer.TAG_IMPORTMODE;

  public final static String SYSPROP_VERSENDEN = IDocumentServiceConstants.SYSPROP_BASE + "." + MetaDocSerializer.TAG_VERSENDEN;

  /**
   * @see org.kalypso.hwv.services.metadoc.DocumentServiceSimulation#commitDocument(java.io.File,
   *      org.kalypsodeegree.model.feature.Feature, java.net.URL, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  protected void commitDocument( final File tmpdir, final Feature metadataFeature, final URL documentURL, final ISimulationMonitor monitor ) throws Exception
  {
    final String preferredFilename = (String) metadataFeature.getProperty( IDocumentServiceConstants.QNAME_META_PREFERRED_FILENAME );

    final String calcCaseName = (String) metadataFeature.getProperty( MetaDocSerializer.QNAME_PROP_CALCCASE );

    final String preferredValidFilename = FileUtilities.validateName( calcCaseName + "_" + preferredFilename, "_" );
    final String goodDocFilePath = filenameCleaner( preferredValidFilename );

    final File docFile = new File( tmpdir, goodDocFilePath );
    final File xmlFile = new File( FileUtilities.nameWithoutExtension( docFile.getAbsolutePath() ) + ".xml" );

    final String importMode = FrameworkProperties.getProperty( SYSPROP_IMPORTMODE, "1" );
    final String versenden = FrameworkProperties.getProperty( SYSPROP_VERSENDEN, "0" );

    Writer writer = null;
    try
    {
      // Copy file
      FileUtils.copyURLToFile( documentURL, docFile );

      // Create XML
      writer = new OutputStreamWriter( new FileOutputStream( xmlFile ), "UTF-8" );
      MetaDocSerializer.buildXML( writer, docFile.getName(), metadataFeature, importMode, versenden );
      writer.close();

      // commit the both files (important: last one is the xml file)
      final String dist = FrameworkProperties.getProperty( SYSPROP_PSICOMPACT_DIST ) + "/";

      final String distDocFile = dist + docFile.getName();
      final String distXmlFile = dist + xmlFile.getName();

      distributeFile( docFile, distDocFile );
      distributeFile( xmlFile, distXmlFile );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
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
    // TODO: create tracing option
    // m_logger.info( "Distributing File " + file.getAbsolutePath() + " to " + distFile );
    /* final boolean b = */PSICompactFactory.getConnection().copyanddistributeFile( file, distFile );
    // TODO: create tracing option
    // m_logger.info( "File distributed: " + b );
  }
}