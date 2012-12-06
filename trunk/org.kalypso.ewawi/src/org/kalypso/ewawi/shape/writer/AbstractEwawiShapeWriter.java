package org.kalypso.ewawi.shape.writer;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.GewWidthShape;
import org.kalypso.model.wspm.ewawi.utils.log.XyzEwawiLogger;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypso.shape.shp.SHPException;

/**
 * Encapsulates some common shape writing code.
 * 
 * @author Holger Albert
 */
public abstract class AbstractEwawiShapeWriter
{
  private final EwawiPlus[] m_data;

  private final GewShape m_gewShape;

  private final GewWidthShape m_gewWidthShape;

  private final String m_outputDirName;

  private final ShapeType m_shapeType;

  public AbstractEwawiShapeWriter( final EwawiPlus[] data, final GewShape gewShape, final GewWidthShape gewWidthShape, final String outputDirName, final ShapeType shapeType )
  {
    m_data = data;
    m_gewShape = gewShape;
    m_gewWidthShape = gewWidthShape;
    m_outputDirName = outputDirName;
    m_shapeType = shapeType;
  }

  public void writeShape( ) throws DBaseException, IOException, SHPException, EwawiException
  {
    /* Create the shape file. */
    final File targetFile = getTargetFile();
    final IDBFField[] fields = createFields();
    final ShapeFile shapeFile = createShapeFile( targetFile, fields );

    /* Create the logger. */
    final XyzEwawiLogger logger = new XyzEwawiLogger();
    logger.init( new File( targetFile.getParentFile(), String.format( "%s.%s", FilenameUtils.removeExtension( targetFile.getName() ), "tab" ) ) );

    /* Write the data. */
    writeData( shapeFile, m_data, logger );

    /* Close the logger. */
    IOUtils.closeQuietly( logger );

    /* Close the shape file. */
    shapeFile.close();

    /* Copy the prj file of the river shape. */
    copyPrj();
  }

  protected EwawiPlus[] getData( )
  {
    return m_data;
  }

  public GewShape getGewShape( )
  {
    return m_gewShape;
  }

  public GewWidthShape getGewWidthShape( )
  {
    return m_gewWidthShape;
  }

  protected File getTargetFile( )
  {
    /* Get the first data object. */
    final EwawiPlus data = m_data[0];

    /* Get the parent folder. */
    final EwawiPro proIndex = data.getProIndex();
    final File sourceFile = proIndex.getSourceFile();
    final String fullPath = FilenameUtils.getFullPath( sourceFile.getAbsolutePath() );
    final String parent = FilenameUtils.normalize( String.format( "%s../%s/", fullPath, m_outputDirName ) ); //$NON-NLS-1$

    /* Create the filename (e.g. ALIAS_VG_BJG_PROFIL_PKT_Freitext.shp). */
    final String filename = getTargetFilename( data.getKey() );

    return new File( parent, filename );
  }

  protected abstract String getTargetFilename( EwawiKey key );

  protected abstract IDBFField[] createFields( ) throws DBaseException;

  protected abstract void writeData( ShapeFile shapeFile, EwawiPlus[] data, XyzEwawiLogger logger ) throws DBaseException, IOException, SHPException, EwawiException;

  private ShapeFile createShapeFile( final File shapeFile, final IDBFField[] fields ) throws DBaseException, IOException
  {
    final String basePath = FilenameUtils.removeExtension( shapeFile.getAbsolutePath() );
    final Charset charset = Charset.defaultCharset();
    return ShapeFile.create( basePath, m_shapeType, charset, fields );
  }

  private void copyPrj( )
  {
    try
    {
      /* Get the file handle of the prj file of the river shape. */
      final GewShape gewShape = getGewShape();
      final File shpFile = gewShape.getFile();
      final File shpPrjFile = new File( shpFile.getParentFile(), String.format( "%s.%s", FilenameUtils.removeExtension( shpFile.getName() ), "prj" ) );

      /* Get the file handle to the target prj file. */
      final File targetFile = getTargetFile();
      final File targetPrjFile = new File( targetFile.getParentFile(), String.format( "%s.%s", FilenameUtils.removeExtension( targetFile.getName() ), "prj" ) );

      /* Copy the file. */
      FileUtils.copyFile( shpPrjFile, targetPrjFile );
    }
    catch( final IOException ex )
    {
      /* We will do this quietly. */
      ex.printStackTrace();
    }
  }
}