/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.ewawi.shape.writer;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.shape.writer.log.XyzEwawiLogger;
import org.kalypso.model.wspm.ewawi.utils.EwawiException;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.GewWidthShape;
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
    logger.init( new File( targetFile.getParentFile(), String.format( "%s.%s", FilenameUtils.removeExtension( targetFile.getName() ), "xyz" ) ) );

    /* Write the data. */
    writeData( shapeFile, m_data, logger );

    /* Close the logger. */
    IOUtils.closeQuietly( logger );

    /* Close the shape file. */
    shapeFile.close();
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
}