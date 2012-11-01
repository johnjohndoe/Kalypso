/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.simulation.statistics;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOCase;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.risk.model.schema.binding.IVectorDataModel;
import org.kalypso.shape.FileMode;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.FieldType;
import org.kalypso.shape.dbf.IDBFField;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Gernot Belger
 */
public class StatisticCalculationData extends AbstractModelObject
{
  private static final File SHAPE_FILE_NONE = new File( "<None>" ); //$NON-NLS-1$

  public static final String PROPERTY_SELECTED_SHAPE = "selectedShape"; //$NON-NLS-1$

  public static final String PROPERTY_SHAPE_ATTRIBUTES = "shapeAttributes"; //$NON-NLS-1$

  public static final String PROPERTY_SELECTED_ATTRIBUTE = "selectedAttribute"; //$NON-NLS-1$

  static final String[] NO_ATTRIBUTES = new String[] { Messages.getString( "StatisticCalculationData_0" ) }; //$NON-NLS-1$

  private final IRasterizationControlModel m_controlModel;

  private final IContainer m_scenarioFolder;

  private final Map<File, String[]> m_availableShapeFiles = new LinkedHashMap<>();

  private File m_selectedShape = null;

  private String m_selectedAttribute = null;

  private final IRasterDataModel m_rasterModel;

  private final IVectorDataModel m_vectorrModel;

  public StatisticCalculationData( final IRasterDataModel rasterModel, final IRasterizationControlModel controlModel, final IVectorDataModel vectorModel, final IContainer scenarioFolder )
  {
    m_rasterModel = rasterModel;
    m_controlModel = controlModel;
    m_vectorrModel = vectorModel;
    m_scenarioFolder = scenarioFolder;
  }

  public void init( )
  {
    initAvailableShapes();
  }

  public IRasterizationControlModel getControlModel( )
  {
    return m_controlModel;
  }

  public IRasterDataModel getRasterModel( )
  {
    return m_rasterModel;
  }

  /**
   * Finds all available polygon shapes and lists their attributes that are either character or numeric.
   */
  private void initAvailableShapes( )
  {
    if( m_scenarioFolder == null )
      return;

    final IProject project = m_scenarioFolder.getProject();
    final IFolder shapeFolder = project.getFolder( "imports" ).getFolder( "basemap" ); //$NON-NLS-1$ //$NON-NLS-2$
    if( !shapeFolder.exists() )
      return;

    final File shapeDir = shapeFolder.getLocation().toFile();
    final IOFileFilter shpFilter = FileFilterUtils.suffixFileFilter( ShapeFile.EXTENSION_SHP, IOCase.INSENSITIVE ); //$NON-NLS-1$
    final File[] shapeFiles = shapeDir.listFiles( (FilenameFilter)shpFilter );
    if( shapeFiles == null )
      return;

    m_availableShapeFiles.put( SHAPE_FILE_NONE, new String[] { StringUtils.EMPTY } );

    for( final File shpFile : shapeFiles )
    {
      final String shpBase = FilenameUtils.removeExtension( shpFile.getAbsolutePath() );

      try( final ShapeFile shapeFile = new ShapeFile( shpBase, Charset.defaultCharset(), FileMode.READ ) )
      {
        final ShapeType shapeType = shapeFile.getShapeType();
        if( shapeType == ShapeType.POLYGON || shapeType == ShapeType.POLYGONZ )
        {
          final Set<String> attributes = new HashSet<>();
          final IDBFField[] fields = shapeFile.getFields();
          for( final IDBFField field : fields )
          {
            final FieldType type = field.getType();
            if( type == FieldType.C || type == FieldType.N )
              attributes.add( field.getName() );
          }

          if( attributes.size() == 0 )
            m_availableShapeFiles.put( shpFile, NO_ATTRIBUTES );
          else
            m_availableShapeFiles.put( shpFile, attributes.toArray( new String[attributes.size()] ) );
        }
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
      catch( final DBaseException e )
      {
        e.printStackTrace();
      }
    }

    m_selectedShape = m_availableShapeFiles.keySet().iterator().next();
  }

  public boolean hasShapes( )
  {
    return m_availableShapeFiles.size() > 1;
  }

  public File[] getAvailableShapeFiles( )
  {
    return m_availableShapeFiles.keySet().toArray( new File[m_availableShapeFiles.size()] );
  }

  public File getSelectedShape( )
  {
    return m_selectedShape;
  }

  public void setSelectedShape( final File selectedShape )
  {
    final File oldValue = m_selectedShape;

    m_selectedShape = selectedShape;

    firePropertyChange( PROPERTY_SELECTED_SHAPE, oldValue, selectedShape );
    firePropertyChange( PROPERTY_SHAPE_ATTRIBUTES, oldValue, selectedShape );

    final String[] attributes = getShapeAttributes();
    if( attributes.length > 0 )
      setSelectedAttribute( attributes[0] );
    else
      setSelectedAttribute( null );
  }

  public String[] getShapeAttributes( )
  {
    return m_availableShapeFiles.get( m_selectedShape );
  }

  public String getSelectedAttribute( )
  {
    return m_selectedAttribute;
  }

  public void setSelectedAttribute( final String selectedAttribute )
  {
    final String oldValue = m_selectedAttribute;

    m_selectedAttribute = selectedAttribute;

    firePropertyChange( PROPERTY_SELECTED_ATTRIBUTE, oldValue, selectedAttribute );
  }

  public ShapeFile loadSelectedShape( ) throws IOException, DBaseException
  {
    if( m_selectedShape == null || m_selectedShape == SHAPE_FILE_NONE )
      return null;

    final String shapeBase = FilenameUtils.removeExtension( m_selectedShape.getAbsolutePath() );
    return new ShapeFile( shapeBase, Charset.defaultCharset(), FileMode.READ );
  }

  public ILandusePolygonCollection getLandusePolygons( )
  {
    return m_vectorrModel.getLandusePolygonCollection();
  }

  public String getShapeSRS( )
  {
    // FIXME: where do we get the srs from?
    // - the map knows it, or,
    // - make sure a .proj (our format) is near the shape (during import)

    return KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
  }
}