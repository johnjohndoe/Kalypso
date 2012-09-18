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

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.Util;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Default {@link AbstractFeatureBinder} based implementation of {@link IWindDataModelSystem}
 *
 * @author ig
 */
public class WindDataModelSystem extends Feature_Impl implements IWindDataModelSystem
{
  public WindDataModelSystem( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private final IFeatureBindingCollection<IWindDataModel> m_windModels = new FeatureBindingCollection<>( this, IWindDataModel.class, KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_MODEL );

  private RectifiedGridDomain m_gridDescriptor;

  /**
   * Creates or return a new feature for the given wind model
   */
  public static final IWindDataModelSystem createWindSystemForWindModel( final IWindModel pWindModel, final RectifiedGridDomain pGridDescriptor, final String strModelSystemName, final String strFileDescription ) throws Exception
  {
    final String lStrCoordinateSystem = pGridDescriptor.getCoordinateSystem();
    final Double lDoubleOriginX = pGridDescriptor.getOrigin( lStrCoordinateSystem ).getPosition().getX();
    final Double lDoubleOriginY = pGridDescriptor.getOrigin( lStrCoordinateSystem ).getPosition().getY();

    final Feature parentFeature = pWindModel;

    final IFeatureBindingCollection<IWindDataModelSystem> lWindDataModelSystem = pWindModel.getWindDataModelSystems();
    final int lIntOrder = lWindDataModelSystem.size();
    Feature lFeature = null;
    try
    {
      lFeature = lWindDataModelSystem.getParentFeature();
    }
    catch( final Exception e )
    {
    }
    if( lFeature != null && lFeature.getName().equalsIgnoreCase( strModelSystemName ) )
    {
      return (IWindDataModelSystem) lFeature;
    }
    else
    {
      lFeature = Util.createFeatureAsProperty( parentFeature, KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_WIND_ELE_SYS, SIM_BASE_F_WIND_ELE_SYS, new Object[] {
          lDoubleOriginX, lDoubleOriginY, pGridDescriptor.getNumColumns(), pGridDescriptor.getNumRows(), pGridDescriptor.getOffsetX( lStrCoordinateSystem ),
          pGridDescriptor.getOffsetY( lStrCoordinateSystem ), lStrCoordinateSystem, strFileDescription, lIntOrder }, new QName[] { QNAME_PROP_ORIGIN_X, QNAME_PROP_ORIGIN_Y, QNAME_PROP_COLUMNS,
          QNAME_PROP_ROWS, QNAME_PROP_CELL_X_LEN, QNAME_PROP_CELL_Y_LEN, QNAME_PROP_CRS, Feature.QN_DESCRIPTION, QNAME_PROP_ORDER } );
      FeatureHelper.addProperty( lFeature, Feature.QN_NAME, strModelSystemName );
    }
    return (IWindDataModelSystem) lFeature;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#getWindDataModels()
   */
  @Override
  public IFeatureBindingCollection<IWindDataModel> getWindDataModels( )
  {
    return m_windModels;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#getGridDescriptor()
   */
  @Override
  public RectifiedGridDomain getGridDescriptor( )
  {
    if( m_gridDescriptor == null )
    {
      m_gridDescriptor = NativeWindDataModelHelper.createGridDescriptor( getOrigin(), getColumns(), getRows(), getCellXLen(), getCellYLen() );
    }
    return m_gridDescriptor;
  }

  private GM_Point getOrigin( )
  {
    final String strCrs = (String) getProperty( QNAME_PROP_CRS );
    final Double lDoubleOriginX = (Double) getProperty( QNAME_PROP_ORIGIN_X );
    final Double lDoubleOriginY = (Double) getProperty( QNAME_PROP_ORIGIN_Y );
    return GeometryFactory.createGM_Point( lDoubleOriginX, lDoubleOriginY, strCrs );
  }

  private double getCellXLen( )
  {
    return (Double) getProperty( QNAME_PROP_CELL_X_LEN );
  }

  private double getCellYLen( )
  {
    return (Double) getProperty( QNAME_PROP_CELL_Y_LEN );
  }

  private int getColumns( )
  {
    return (Integer) getProperty( QNAME_PROP_COLUMNS );
  }

  private int getRows( )
  {
    return (Integer) getProperty( QNAME_PROP_ROWS );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#getOrder()
   */
  @Override
  public int getOrder( )
  {
    return (Integer) getProperty( QNAME_PROP_ORDER );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#setOrder(int)
   */
  @Override
  public void setOrder( final int pOrder )
  {
    setProperty( QNAME_PROP_ORDER, pOrder );
  }

}
