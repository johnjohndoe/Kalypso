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
package org.kalypso.ui.rrm.internal.newproject;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sourceforge.nattable.util.ArrayUtil;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.WizardPage;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.shape.ShapeType;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Data (and binding) object for the Kalypso RRM project wizard.
 *
 * @author Gernot Belger
 */
public class KalypsoNAMappingData extends AbstractModelObject
{
  public static final String PROPERTY_SHAPE_TYPE_STATUS = "shapeTypeStatus"; //$NON-NLS-1$

  private static final IStatus SHAPE_TYPE_STATUS_NONE = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "No shape file loaded" );

  private static final IStatus SHAPE_TYPE_STATUS_LOADING = new Status( IStatus.INFO, KalypsoUIRRMPlugin.getID(), "Loading shape file..." );

  private String m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private final Set<ShapeType> m_allowedShapeTypes = new HashSet<>();

  private File m_shapeFile;

  private boolean m_skip = true;

  private IStatus m_shapeTypeStatus = SHAPE_TYPE_STATUS_NONE;

  private final Map<IValuePropertyType, IValuePropertyType> m_mapping = new HashMap<IValuePropertyType, IValuePropertyType>();

  private FeatureList m_sourceData;

  private final IFeatureType m_targetFeatureType;

  private final IValuePropertyType[] m_targetProperties;

  public KalypsoNAMappingData( final ShapeType[] allowedTypes, final IFeatureType targetFeatureType, final IValuePropertyType[] targetProperties )
  {
    m_targetFeatureType = targetFeatureType;
    m_targetProperties = targetProperties;
    m_allowedShapeTypes.addAll( ArrayUtil.asList( allowedTypes ) );
  }

  public IValuePropertyType[] getTargetProperties( )
  {
    return m_targetProperties;
  }

  public boolean getSkip( )
  {
    return m_skip;
  }

  public void setSkip( final boolean skip )
  {
    m_skip = skip;
  }

  public void setSrs( final String srs )
  {
    m_crs = srs;
  }

  public String getSrs( )
  {
    return m_crs;
  }

  public void setShapeFile( final File file )
  {
    m_shapeFile = file;
  }

  public File getShapeFile( )
  {
    return m_shapeFile;
  }

  public void readShapeFile( final WizardPage page )
  {
    final GMLWorkspace workspace = loadShapeFile( page );

    if( m_sourceData != null )
    {
      m_sourceData.getOwner().getWorkspace().dispose();
      m_sourceData = null;
    }

    if( workspace != null )
    {
      final Feature rootFeature = workspace.getRootFeature();
      m_sourceData = (FeatureList) rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );
    }
  }

  private GMLWorkspace loadShapeFile( final WizardPage page )
  {
    setShapeTypeStatus( SHAPE_TYPE_STATUS_NONE );

    final File shapeFile = getShapeFile();
    if( shapeFile == null )
      return null;

    final String shapePath = shapeFile.getAbsolutePath();
    final String fileBase = FilenameUtils.removeExtension( shapePath );

    try
    {
      setShapeTypeStatus( SHAPE_TYPE_STATUS_LOADING );

      final String srs = getSrs();
      final GMLWorkspace workspace = ShapeSerializer.deserialize( fileBase, srs );

      final IStatus typeStatus = checkShapeType( workspace );
      setShapeTypeStatus( typeStatus );

      if( typeStatus.matches( IStatus.ERROR ) )
        page.setPageComplete( false );

      return workspace;
    }
    catch( final GmlSerializeException e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString( "KalypsoNAProjectWizardPage_1" ), e.getLocalizedMessage() ); //$NON-NLS-1$

      page.setPageComplete( false );

      final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, msg, e );

      setShapeTypeStatus( status );

      return null;
    }
  }

  private IStatus checkShapeType( final GMLWorkspace workspace )
  {
    final Feature rootFeature = workspace.getRootFeature();

    final Integer actualTypeNumber = (Integer) rootFeature.getProperty( ShapeSerializer.PROPERTY_TYPE );
    final ShapeType actualType = ShapeType.valueOf( actualTypeNumber );
    final String typeName = actualType.getLabel();

    if( m_allowedShapeTypes.contains( actualType ) )
    {
      final String message = String.format( "Geometry valid: %s", typeName );
      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), message );
    }

    final Collection<String> allowedNames = new ArrayList<>();
    for( final ShapeType allowedType : m_allowedShapeTypes )
      allowedNames.add( allowedType.getLabel() );

    final String allAllowedTypes = StringUtils.join( allowedNames, ',' );

    final String message = String.format( "Invalid geometry type (%s). Allowed types are: %s", typeName, allAllowedTypes );
    return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message );
  }

  private void setShapeTypeStatus( final IStatus status )
  {
    final IStatus oldValue = m_shapeTypeStatus;

    m_shapeTypeStatus = status;

    firePropertyChange( PROPERTY_SHAPE_TYPE_STATUS, oldValue, status );
  }

  public IStatus getShapeTypeStatus( )
  {
    return m_shapeTypeStatus;
  }

  public void removeMapping( final Object targetPT )
  {
    m_mapping.remove( targetPT );
  }

  public void setMapping( final IValuePropertyType targetPT, final IValuePropertyType firstElement )
  {
    m_mapping.put( targetPT, firstElement );
  }

  public IValuePropertyType[] getSourceProperties( )
  {
    if( m_sourceData == null )
      return null;

    final List<IValuePropertyType> result = new ArrayList<IValuePropertyType>();

    final IFeatureType srcFT = m_sourceData.getPropertyType().getTargetFeatureType();
    final IPropertyType[] ftp = srcFT.getProperties();

    for( final IPropertyType element : ftp )
    {
      if( element instanceof IValuePropertyType )
      {
        final IValuePropertyType fromPT = (IValuePropertyType) element;
        if( !fromPT.isGeometry() )
          result.add( fromPT );
      }
    }

    return result.toArray( new IValuePropertyType[result.size()] );
  }

  public Map<IValuePropertyType, IValuePropertyType> getMapping( )
  {
    if( getSkip() )
      return null;

    final Map<IValuePropertyType, IValuePropertyType> clone = new HashMap<>( m_mapping );

    /* add geometry mapping by default */
    final IValuePropertyType targetGeometry = m_targetFeatureType.getDefaultGeometryProperty();
    final IValuePropertyType sourceGeometry = m_sourceData.getPropertyType().getTargetFeatureType().getDefaultGeometryProperty();
    clone.put( targetGeometry, sourceGeometry );

    return clone;
  }

  public List< ? > getSourceData( )
  {
    return m_sourceData;
  }
}