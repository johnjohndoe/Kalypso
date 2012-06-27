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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashSet;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.databinding.observable.set.WritableSet;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;

/**
 * @author Gernot Belger
 */
public class ImportWaterBodiesData extends AbstractModelObject
{
  public static enum INSERTION_MODE
  {
    skip(Messages.getString( "ImportWaterBodiesData.0" )), //$NON-NLS-1$
    overwrite(Messages.getString( "ImportWaterBodiesData.1" )); //$NON-NLS-1$

    private final String m_label;

    private INSERTION_MODE( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  public static final String PROPERTY_WATER_BODIES = "waterBodies"; //$NON-NLS-1$

  public static final String PROPERTY_INSERTION_MODE = "insertionMode"; //$NON-NLS-1$

  private String m_srs;

  private String m_shapeFile;

  private WaterBody[] m_waterBodies;

  private INSERTION_MODE m_insertionMode;

  private final WritableSet m_selectedWaterBodies;

  private WaterBody[] m_existingWaterbodies;

  private ImportAttributeInfo< ? >[] m_infos;

  private int m_targetSRID;

  public ImportWaterBodiesData( )
  {
    m_srs = null;
    m_shapeFile = null;
    m_waterBodies = null;
    m_insertionMode = INSERTION_MODE.skip;
    m_selectedWaterBodies = new WritableSet( new HashSet<WaterBody>(), WaterBody.class );
    m_existingWaterbodies = null;
    m_infos = null;
  }

  public void setShapeInput( final String shapeFile, final String srs )
  {
    if( shapeFile == null )
      m_shapeFile = null;
    else
    {
      if( shapeFile.toLowerCase().endsWith( ".shp" ) ) //$NON-NLS-1$
        m_shapeFile = FilenameUtils.removeExtension( shapeFile );
      else
        m_shapeFile = shapeFile;
    }

    m_srs = srs;
  }

  public String getShapeFile( )
  {
    return m_shapeFile;
  }

  public ImportAttributeInfo< ? >[] getAttributeInfos( )
  {
    return m_infos;
  }

  public void setWaterBodies( final WaterBody[] waterBodies )
  {
    final Object oldValue = m_waterBodies;

    m_waterBodies = waterBodies;

    firePropertyChange( PROPERTY_WATER_BODIES, waterBodies, oldValue );
  }

  public WaterBody[] getWaterBodies( )
  {
    return m_waterBodies;
  }

  public ShapeFile openShape( ) throws IOException, DBaseException
  {
    final String basePath = getShapeFile();
    return new ShapeFile( basePath, Charset.defaultCharset(), FileMode.READ );
  }

  public String getShapeSrs( )
  {
    return m_srs;
  }

  public WritableSet getSelectedWaterBodies( )
  {
    return m_selectedWaterBodies;
  }

  public INSERTION_MODE getInsertionMode( )
  {
    return m_insertionMode;
  }

  public void setInsertionMode( final INSERTION_MODE insertionMode )
  {
    final INSERTION_MODE oldValue = m_insertionMode;

    m_insertionMode = insertionMode;

    firePropertyChange( PROPERTY_INSERTION_MODE, oldValue, m_insertionMode );
  }

  public void setExistingWaterBodies( final WaterBody[] existingWaterBodies )
  {
    m_existingWaterbodies = existingWaterBodies;
  }

  public WaterBody[] getExistingWaterBodies( )
  {
    return m_existingWaterbodies;
  }

  public void setAttributeInfos( final ImportAttributeInfo< ? >[] infos )
  {
    m_infos = infos;
  }

  /** The water bodies will transformed to this SRS when read from the shape file */
  public int getTargetSRID( )
  {
    return m_targetSRID;
  }

  public void setTargetSRID( final int targetSRID )
  {
    m_targetSRID = targetSRID;
  }
}