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
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Date;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.deegree.framework.util.Pair;
import org.eclipse.core.resources.IFile;
import org.kalypso.afgui.model.Util;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
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
  private final IWindDataProvider m_windDataProvider;

  private IWindDataModelSystem m_windDataModelSystem;

  private final IFile m_file;

  private Date m_date = null;

  /**
   * Creates a new {@link NativeWindDataModelWrapper} as child of the given parent and link to it by the prop of the
   * given name. The native source path is also set
   */
  @SuppressWarnings("deprecation")
  private static final Feature createFeature( final Feature parentFeature, final QName propQName, final String sourceName, final Date date ) throws Exception
  {
    final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( date );
    String lStrDate = ( new Date() ).toGMTString();
    final Feature newFeature = Util.createFeatureAsProperty( parentFeature, propQName, KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER, new Object[] { sourceName,
        gregorianCalendar, parentFeature.getName() + " imported on " + lStrDate }, new QName[] { KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_FILE_NAME, QNAME_PROP_DATE, Feature.QN_DESCRIPTION } );
    return newFeature;
  }

  public NativeWindDataModelWrapper( final IWindDataModelSystem pWindDataModelSystem, final String sourceName, final Date date ) throws Exception
  {
    this( createFeatureForTEMSystem( pWindDataModelSystem, sourceName, date ) );
    m_windDataModelSystem = pWindDataModelSystem;
  }

  private static final Feature createFeatureForTEMSystem( final IWindDataModelSystem pWindDataModelSystem, final String sourceName, final Date date ) throws Exception
  {
    return createFeature( pWindDataModelSystem.getFeature(), KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_MODEL, sourceName, date );
  }

  public NativeWindDataModelWrapper( final Feature featureToBind ) throws Exception
  {
    super( featureToBind, KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER );

    final String sourceName = (String) featureToBind.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_FILE_NAME );
    if( sourceName == null )
    {
      throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.windmodel.NativeWindDataModelWrapper.2" ) ); //$NON-NLS-1$
    }

    // m_strCrs = (String) featureToBind.getProperty( QNAME_PROP_CRS );
    //
    try
    {
      Feature lFeatureWindDataModelSystem = featureToBind.getParent();
      m_windDataModelSystem = (IWindDataModelSystem) lFeatureWindDataModelSystem.getAdapter( IWindDataModelSystem.class );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    m_date = DateUtilities.toDate( (XMLGregorianCalendar) featureToBind.getProperty( QNAME_PROP_DATE ) );

    final URL sourceURL = makeSourceURL( sourceName, featureToBind.getWorkspace().getContext() );

    m_file = ResourceUtilities.findFileFromURL( sourceURL );
    m_windDataProvider = NativeWindDataModelHelper.getWindDataModel( m_file.getLocation().toFile(), getGridDescriptor() );
  }

  private final URL makeSourceURL( final String sourceName, final URL worspaceContex ) throws URISyntaxException, MalformedURLException
  {
    try
    {
      final URL sourceUrl = new URL( sourceName );
      final URI uri = sourceUrl.toURI();
      if( uri.isAbsolute() )
      {
        return uri.toURL();
      }
      else
      {
        final URL absURL = new URL( worspaceContex, sourceName );
        return absURL;
      }
    }
    catch( final MalformedURLException e )
    {
      return new URL( worspaceContex, sourceName );
    }
  }

  public IWindDataProvider getWindDataProvider( )
  {
    return m_windDataProvider;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_windDataProvider.getBoundingBox();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getCoordinateSystem()
   */
  public String getCoordinateSystem( ) throws Exception
  {
    return m_windDataProvider.getCoordinateSystem();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#setCoordinateSystem(java.lang.String)
   */
  public void setCoordinateSystem( final String coordinateSystem )
  {
    m_windDataProvider.setCoordinateSystem( coordinateSystem );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel#getWindDataModelSystem()
   */
  @Override
  public IWindDataModelSystem getWindDataModelSystem( )
  {
    return m_windDataModelSystem;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getWindAsSpeedAndDirection(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair<Double, Double> getWindAsSpeedAndDirection( GM_Point location )
  {
    return m_windDataProvider.getWindAsSpeedAndDirection( location );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getWindAsVector(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public Pair<Double, Double> getWindAsVector( GM_Point location )
  {
    return m_windDataProvider.getWindAsVector( location );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getDataAsGrid()
   */
  @Override
  public IGeoGrid getDataAsGrid( )
  {
    return m_windDataProvider.getDataAsGrid();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#getGridDescriptor()
   */
  @Override
  public RectifiedGridDomain getGridDescriptor( ) throws Exception
  {
    return m_windDataModelSystem.getGridDescriptor();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider#isRegularGrid()
   */
  @Override
  public boolean isRegularGrid( )
  {
    return m_windDataProvider.isRegularGrid();
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
      lUrlDataFile = m_file.getLocationURI().toURL();
    }
    catch( Exception e )
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
    return m_date != null? m_date: m_windDataProvider.getDateStep();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.INativeWindDataModelWrapper#getSourceFile()
   */
  @Override
  public IFile getSourceFile( )
  {
    return m_file;
  }

}
