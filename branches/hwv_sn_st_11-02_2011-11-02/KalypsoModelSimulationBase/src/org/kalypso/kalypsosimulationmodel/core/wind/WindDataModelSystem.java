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
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Default {@link AbstractFeatureBinder} based implementation of {@link IWindDataModelSystem}
 * 
 * @author ig
 */
public class WindDataModelSystem extends AbstractFeatureBinder implements IWindDataModelSystem
{
  private GM_Point m_gmPointOrigin;

  private double m_doubleCellXLen;

  private double m_doubleCellYLen;

  private int m_intColumns;

  private int m_intRows;

  private Integer m_intOrder = -1;

  private String m_strCrs;

  private final IFeatureWrapperCollection<IWindDataModel> m_windModels;

  private RectifiedGridDomain m_gridDescriptor;


  /**
   * Creates a {@link WindDataModelSystem} object binding the given feature. the given feature must be substitutable to
   * simBase:WindDataModelSystem
   * 
   * @throws IllegalArgumentException
   *           if featureToBind is null or not substitutable to simBase:WindDataModel
   */
  public WindDataModelSystem( final Feature featureToBind ) throws IllegalArgumentException
  {
    super( featureToBind, KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS );
    m_windModels = new FeatureWrapperCollection<IWindDataModel>( featureToBind, IWindDataModel.class, KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_MODEL );
    try
    {

      m_strCrs = (String) featureToBind.getProperty( QNAME_PROP_CRS );

      Double lDoubleOriginX = (Double) featureToBind.getProperty( QNAME_PROP_ORIGIN_X );
      Double lDoubleOriginY = (Double) featureToBind.getProperty( QNAME_PROP_ORIGIN_Y );

      m_gmPointOrigin = GeometryFactory.createGM_Point( lDoubleOriginX, lDoubleOriginY, m_strCrs );

      m_doubleCellXLen = (Double) featureToBind.getProperty( QNAME_PROP_CELL_X_LEN );

      m_doubleCellYLen = (Double) featureToBind.getProperty( QNAME_PROP_CELL_Y_LEN );

      m_intColumns = (Integer) featureToBind.getProperty( QNAME_PROP_COLUMNS );

      m_intRows = (Integer) featureToBind.getProperty( QNAME_PROP_ROWS );

      m_intOrder = (Integer) featureToBind.getProperty( QNAME_PROP_ORDER );
    }
    catch( Exception e )
    {
    }
  }

  public WindDataModelSystem( final IWindModel pWindModel, final RectifiedGridDomain pGridDescriptor, final String strModelSystemName, final String strFileDescription ) throws Exception
  {
    this( createWindSystemForWindModel( pWindModel, pGridDescriptor, strModelSystemName, strFileDescription ) );
  }

  /**
   * Creates or return a new feature for the given wind model
   */
  private static final Feature createWindSystemForWindModel( final IWindModel pWindModel, final RectifiedGridDomain pGridDescriptor, final String strModelSystemName, final String strFileDescription ) throws Exception
  {
    String lStrCoordinateSystem = pGridDescriptor.getCoordinateSystem();
    Double lDoubleOriginX = pGridDescriptor.getOrigin( lStrCoordinateSystem ).getPosition().getX();
    Double lDoubleOriginY = pGridDescriptor.getOrigin( lStrCoordinateSystem ).getPosition().getY();

    final Feature parentFeature = pWindModel.getFeature();

    final IFeatureWrapperCollection<IWindDataModelSystem> lWindDataModelSystem = pWindModel.getWindDataModelSystems();
    int lIntOrder = lWindDataModelSystem.size();
    Feature lFeature = null;
    try
    {
      lFeature = lWindDataModelSystem.getFeature();
    }
    catch( Exception e )
    {
    }
    if( lFeature != null && lFeature.getName().equalsIgnoreCase( strModelSystemName ) )
    {
      return lFeature;
    }
    else
    {
      lFeature = Util.createFeatureAsProperty( parentFeature, KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_WIND_ELE_SYS, KalypsoModelSimulationBaseConsts.SIM_BASE_F_WIND_ELE_SYS, new Object[] {
          lDoubleOriginX, lDoubleOriginY, pGridDescriptor.getNumColumns(), pGridDescriptor.getNumRows(), pGridDescriptor.getOffsetX( lStrCoordinateSystem ),
          pGridDescriptor.getOffsetY( lStrCoordinateSystem ), lStrCoordinateSystem, strFileDescription, lIntOrder }, new QName[] { QNAME_PROP_ORIGIN_X, QNAME_PROP_ORIGIN_Y, QNAME_PROP_COLUMNS, QNAME_PROP_ROWS,
          QNAME_PROP_CELL_X_LEN, QNAME_PROP_CELL_Y_LEN, QNAME_PROP_CRS, Feature.QN_DESCRIPTION, QNAME_PROP_ORDER } );
      FeatureHelper.addProperty( lFeature, Feature.QN_NAME, strModelSystemName );
    }
    return lFeature;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#getWindDataModels()
   */
  @Override
  public IFeatureWrapperCollection<IWindDataModel> getWindDataModels( )
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
      m_gridDescriptor = NativeWindDataModelHelper.createGridDescriptor( m_gmPointOrigin, m_intColumns, m_intRows, m_doubleCellXLen, m_doubleCellYLen );
    }
    return m_gridDescriptor;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#getOrder()
   */
  @Override
  public int getOrder( )
  {
    return m_intOrder;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem#setOrder(int)
   */
  @Override
  public void setOrder( int pOrder )
  {
    this.getFeature().setProperty( QNAME_PROP_ORDER, pOrder );
  }
  
  
}
