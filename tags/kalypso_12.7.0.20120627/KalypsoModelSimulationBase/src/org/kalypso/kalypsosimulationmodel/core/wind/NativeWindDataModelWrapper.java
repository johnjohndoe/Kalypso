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
package org.kalypso.kalypsosimulationmodel.core.wind;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.deegree.framework.util.Pair;
import org.eclipse.core.resources.IFile;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * Provide the implementation of {@link IWindDataModel} for simBase:NativeWindDataModelWrapper model.
 *
 * @author ig
 *
 */
public class NativeWindDataModelWrapper extends WindDataModel implements INativeWindDataModelWrapper
{
  public NativeWindDataModelWrapper( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private IWindDataProvider m_windDataProvider;

  /**
   * Creates a new {@link NativeWindDataModelWrapper} as child of the given parent and link to it by the prop of the
   * given name. The native source path is also set
   */
  private static final INativeWindDataModelWrapper createFeature( final Feature parentFeature, final QName propQName, final String sourceName, final Date date ) throws Exception
  {
    final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( date );
    final String lStrDate = (new Date()).toGMTString();
    final Feature newFeature = Util.createFeatureAsProperty( parentFeature, propQName, SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER, new Object[] { sourceName, gregorianCalendar,
        parentFeature.getName() + Messages.getString("NativeWindDataModelWrapper.0") + lStrDate }, new QName[] { KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_FILE_NAME, QNAME_PROP_DATE, Feature.QN_DESCRIPTION } ); //$NON-NLS-1$
    return (INativeWindDataModelWrapper) newFeature;
  }

  public static final INativeWindDataModelWrapper createFeatureForTEMSystem( final IWindDataModelSystem pWindDataModelSystem, final String sourceName, final Date date ) throws Exception
  {
    return createFeature( pWindDataModelSystem, KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_MODEL, sourceName, date );
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

  public IWindDataProvider getWindDataProvider( )
  {
    if( m_windDataProvider != null )
      return m_windDataProvider;

    final IFile file = getSourceFile();
    if( file == null )
    {
      throw new IllegalArgumentException( "" ); //$NON-NLS-1$
    }
    try
    {
      m_windDataProvider = NativeWindDataModelHelper.getWindDataModel( file.getLocation().toFile(), getGridDescriptor() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return m_windDataProvider;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    return getWindDataProvider().getBoundingBox();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( ) throws Exception
  {
    return getWindDataProvider().getCoordinateSystem();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    getWindDataProvider().setCoordinateSystem( coordinateSystem );
  }

  @Override
  public IWindDataModelSystem getWindDataModelSystem( )
  {
    return (IWindDataModelSystem) getOwner();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getWindAsSpeedAndDirection(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair<Double, Double> getWindAsSpeedAndDirection( final GM_Point location )
  {
    return getWindDataProvider().getWindAsSpeedAndDirection( location );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getWindAsVector(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair<Double, Double> getWindAsVector( final GM_Point location )
  {
    return getWindDataProvider().getWindAsVector( location );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataAsGrid()
   */
  @Override
  public IGeoGrid getDataAsGrid( )
  {
    return getWindDataProvider().getDataAsGrid();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getGridDescriptor()
   */
  @Override
  public RectifiedGridDomain getGridDescriptor( ) throws Exception
  {
    return getWindDataModelSystem().getGridDescriptor();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#isRegularGrid()
   */
  @Override
  public boolean isRegularGrid( )
  {
    return getWindDataProvider().isRegularGrid();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataFile()
   */
  @Override
  public URL getDataFileURL( )
  {
    URL lUrlDataFile = null;
    try
    {
      lUrlDataFile = getSourceFile().getLocationURI().toURL();
    }
    catch( final Exception e )
    {
    }
    return lUrlDataFile;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDateStep()
   */
  @Override
  public Date getDateStep( )
  {
    final Date date = DateUtilities.toDate( (XMLGregorianCalendar) getProperty( QNAME_PROP_DATE ) );
    return date != null ? date : getWindDataProvider().getDateStep();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.INativeWindDataModelWrapper#getSourceFile()
   */
  @Override
  public IFile getSourceFile( )
  {
    final String sourceName = (String) getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_FILE_NAME );

    // why using urls and file? We live in an eclipse workspace! Use IRources
    final URL sourceURL = makeSourceURL( sourceName, this.getWorkspace().getContext() );
    final IFile file = ResourceUtilities.findFileFromURL( sourceURL );

    return file;
  }

}
