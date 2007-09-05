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
package org.kalypso.kalypsomodel1d2d.sim;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.ogc.gml.serialize.AbstractFeatureProviderFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerXlinkFeatureProvider;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureProvider;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 */
public class RMA10Calculation
{
  private GMLWorkspace m_discretisationModelWorkspace = null;

  private GMLWorkspace m_flowRelationshipsWorkspace = null;

  private Feature m_controlModelRoot = null;

  private Feature m_roughnessRootWorkspace = null;

  private String m_kalypso1D2DKernelPath;

  private ICalculationUnit m_calculationUnit;

  private IControlModel1D2D m_controlModel1D2D = null;

  private List<IBoundaryCondition> m_unitBoundaryConditions;

  public RMA10Calculation( final ISimulationDataProvider inputProvider ) throws SimulationException, Exception
  {
    final Map<String, String> specialUrls = new HashMap<String, String>();
    specialUrls.put( "project:/.metadata/roughness.gml", "Roughness" );

    final IUrlResolver resolver = new UrlResolver()
    {
      /**
       * @see org.kalypso.contribs.java.net.UrlResolver#resolveURL(java.net.URL, java.lang.String)
       */
      @Override
      public URL resolveURL( URL baseURL, String relativeURL ) throws MalformedURLException
      {
        if( specialUrls.containsKey( relativeURL ) )
        {
          final String specialUrl = specialUrls.get( relativeURL );
          try
          {
            return (URL) inputProvider.getInputForID( specialUrl );
          }
          catch( final SimulationException e )
          {
            e.printStackTrace();
            throw new MalformedURLException( "Missing input for ID: " + specialUrl );
          }
        }

        return super.resolveURL( baseURL, relativeURL );
      }
    };

    final IFeatureProviderFactory factory = new AbstractFeatureProviderFactory()
    {
      @Override
      protected IFeatureProvider createProvider( Feature context, String uri, String role, String arcrole, String title, String show, String actuate )
      {
        return new GmlSerializerXlinkFeatureProvider( context, uri, role, arcrole, title, show, actuate, this, resolver );
      }
    };

    m_discretisationModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.DISCRETISATIOMODEL_ID ), factory );
    m_flowRelationshipsWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.FLOWRELATIONSHIPMODEL_ID ), factory );
    m_controlModelRoot = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.CONTROL_ID ), factory ).getRootFeature();
    m_roughnessRootWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.ROUGHNESS_ID ), factory ).getRootFeature();

    init();
  }

  private void init( ) throws SimulationException
  {
    getControlModel();
    getCalculationUnit();
    initializeInfos();
  }

  public IFlowRelationshipModel getFlowModel( )
  {
    return (IFlowRelationshipModel) m_flowRelationshipsWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );
  }

  public IFEDiscretisationModel1d2d getDiscModel( )
  {
    return (IFEDiscretisationModel1d2d) m_discretisationModelWorkspace.getRootFeature().getAdapter( IFEDiscretisationModel1d2d.class );
  }

  public String getKalypso1D2DKernelPath( )
  {
    final IControlModel1D2D controlModel = getControlModel();
    final String kalypso1D2DVersion = controlModel.getVersion();

    if( kalypso1D2DVersion.equals( "test" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_TEST;
    else if( kalypso1D2DVersion.equals( "NEW" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
    else if( kalypso1D2DVersion.equals( "Version3.5" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
    else
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;

    return m_kalypso1D2DKernelPath;
  }

  public Double[] getViskosity( final Feature roughnessFE )
  {
    final Double[] result = new Double[4];
    final int iedsw = getControlModel().getIEDSW();

    // turbulence combo (iedsw):
    if( iedsw == 0 || iedsw == 10 || iedsw == 13 )
    {
      final double defaultValue = 2900.0;
      result[0] = defaultValue;
      result[1] = defaultValue;
      result[2] = defaultValue;
      result[3] = defaultValue;
      // result[0] = getEddyXX( roughnessFE );
      // result[1] = getEddyYX( roughnessFE );
      // result[2] = getEddyXY( roughnessFE );
      // result[3] = getEddyYY( roughnessFE );
    }
    else
    {
      // TODO: strange values, check again with Nico
      // final double defaultValue = 0.4;
      final double defaultValue = 2900.0;
      result[0] = defaultValue;
      result[1] = defaultValue;
      result[2] = defaultValue;
      result[3] = defaultValue;
    }
    return result;
    // return getcharactV( roughnessFE ); 0.4
  }

  public Double getEddyXX( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XX, 500.0 );
  }

  public Double getEddyYX( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YX, 500.0 );
  }

  public Double getEddyXY( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_XY, 500.0 );
  }

  public Double getEddyYY( final Feature roughnessFE )
  {
    return FeatureHelper.getAsDouble( roughnessFE, KalypsoModelRoughnessConsts.WBR_PROP_EDDY_YY, 500.0 );
  }

  public Double getcharactV( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV );
  }

  public String getName( final Feature feature )
  {
    return (String) feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME );
  }

  public List getRoughnessClassList( )
  {
    return (List) m_roughnessRootWorkspace.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_ROUGHNESS_CLS_MEMBER );
  }

  public Double getKs( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS );
  }

  public Double getAxAy( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY );
  }

  public Double getDp( final Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP );
  }

  private void initializeInfos( )
  {
    final IFlowRelationshipModel flowRelationshipsModel = (IFlowRelationshipModel) m_flowRelationshipsWorkspace.getRootFeature().getAdapter( IFlowRelationshipModel.class );
    m_unitBoundaryConditions = new ArrayList<IBoundaryCondition>();
    for( final IFlowRelationship relationship : flowRelationshipsModel )
      if( relationship instanceof IBoundaryCondition )
      {
        final IBoundaryCondition boundaryCondition = (IBoundaryCondition) relationship;
        if( isBoundaryConditionMemberOfCalculationUnit( boundaryCondition ) )
          m_unitBoundaryConditions.add( boundaryCondition );
      }
  }

  private boolean isBoundaryConditionMemberOfCalculationUnit( final IBoundaryCondition boundaryCondition )
  {
    final List<String> parentCalculationUnits = boundaryCondition.getParentCalculationUnitIDs();
    for( final String parentCalculationUnit : parentCalculationUnits )
      if( m_calculationUnit.getGmlID().equals( parentCalculationUnit ) )
        return true;
    return false;
  }

  public List<IBoundaryCondition> getBoundaryConditions( )
  {
    return m_unitBoundaryConditions;
  }

  public List<IFELine> getContinuityLines( )
  {
    return m_calculationUnit.getContinuityLines();
  }

  public ICalculationUnit getCalculationUnit( )
  {
    if( m_calculationUnit == null )
      m_calculationUnit = getControlModel().getCalculationUnit();
    return m_calculationUnit;
  }

  public IControlModel1D2D getControlModel( )
  {
    if( m_controlModel1D2D == null )
    {
      final Feature controlModelCollection = (Feature) m_controlModelRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_FP_MODEL_COLLECTION );
      final Object link = controlModelCollection.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_XP_ACTIVE_MODEL );
      final Feature controlModelFeature = FeatureHelper.getFeature( m_controlModelRoot.getWorkspace(), link );
      m_controlModel1D2D = (IControlModel1D2D) controlModelFeature.getAdapter( IControlModel1D2D.class );
    }
    return m_controlModel1D2D;
  }
}
