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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogModelSimulationBase;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Provide the implementation of {@link ITerrainElevationModel} for simBase:NativeTerrainElevationModelWrapper model.
 * This class collaborates with ...
 * 
 * @author Madanagopal
 * @author Patrice Congo
 * 
 */
public class NativeTerrainElevationModelWrapper extends TerrainElevationModel implements INativeTerrainElevationModelWrapper
{
  public static final QName SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "NativeTerrainElevationModelWrapper" ); //$NON-NLS-1$

  public static final QName SIM_BASE_F_BASE_TERRAIN_ELE_MODEL = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "_TerrainElevationModel" ); //$NON-NLS-1$

  public static final QName SIM_BASE_PROP_FILE_NAME = new QName( UrlCatalogModelSimulationBase.SIM_MODEL_NS, "fileName" ); //$NON-NLS-1$

  private IElevationProvider m_elevationProvider;

  public NativeTerrainElevationModelWrapper( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  public IElevationProvider getElevationProvider( )
  {
    if( m_elevationProvider != null )
      return m_elevationProvider;

    final IFile file = getSourceFile();
    if( file == null )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper.2" ) ); //$NON-NLS-1$
    }
    try
    {
      m_elevationProvider = NativeTerrainElevationModelFactory.getTerrainElevationModel( file.getLocation().toFile() );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    return m_elevationProvider;
  }

  private final URL makeSourceURL( final String sourceName, final URL worspaceContex )
  {
    try
    {
      return UrlResolverSingleton.resolveUrl( worspaceContex, sourceName );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getElevation( final GM_Point location )
  {
    return getElevationProvider().getElevation( location );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    return getElevationProvider().getBoundingBox();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return getElevationProvider().getCoordinateSystem();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   */
  @Override
  public double getMaxElevation( )
  {
    return getElevationProvider().getMaxElevation();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   */
  @Override
  public double getMinElevation( )
  {
    return getElevationProvider().getMinElevation();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper#getSourceFile()
   */
  @Override
  public IFile getSourceFile( )
  {
    final String sourceName = (String) getProperty( SIM_BASE_PROP_FILE_NAME );

    // why using urls and file? We live in an eclipse workspace! Use IRources
    final URL sourceURL = makeSourceURL( sourceName, this.getWorkspace().getContext() );
    final IFile file = ResourceUtilities.findFileFromURL( sourceURL );

    return file;
  }

  public void setFile( String filename )
  {
    setProperty( SIM_BASE_PROP_FILE_NAME, filename );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    // TODO

  }
}
