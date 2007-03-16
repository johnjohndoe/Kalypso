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
package org.kalypso.kalypsomodel1d2d.sim;

import java.net.URL;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.simulation.core.ISimulationDataProvider;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author huebsch <a href="mailto:j.huebsch@tuhh.de">Jessica Huebsch</a>
 */
public class RMA10Calculation
{

  private GMLWorkspace m_flowResWS = null;

  private GMLWorkspace m_flowRelWS = null;

  private GMLWorkspace m_disModelWorkspace = null;

  private GMLWorkspace m_operationalWS = null;

  private Feature m_controlRoot = null;

  private Feature m_roughnessRoot = null;

  private boolean m_restart;

  private String m_kalypso1D2DKernelPath;

  public RMA10Calculation( final ISimulationDataProvider inputProvider)
  {
    try
    {
      // m_flowResWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID(
      // RMA10SimModelConstants.FLOWRESISTANCEMODEL_ID ),
      // null );
      // m_flowRelWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID(
      // RMA10SimModelConstants.FLOWRELATIONSHIPMODEL_ID ),
      // null );
      m_disModelWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.DISCRETISATIOMODEL_ID ), null );
      // m_operationalWS = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID(
      // RMA10SimModelConstants.OPERATIONALMODEL_ID ),
      // null );
      m_controlRoot = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.CONTROL_ID ), null ).getRootFeature();
      m_roughnessRoot = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID( RMA10SimModelConstants.ROUGHNESS_ID ), null ).getRootFeature();
      // final GMLWorkspace simResWorkspace = GmlSerializer.createGMLWorkspace( (URL) inputProvider.getInputForID(
      // RMA10SimModelConstants.SIMULATIONRESULTMODEL_ID ), null );

    }
    catch( SimulationException e )
    {
      e.printStackTrace();
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  public GMLWorkspace getDisModelWorkspace( )
  {
    return m_disModelWorkspace;
  }

  public void setKalypso1D2DKernelPath( )
  {
    String kalypso1D2DVersion = (String) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VERSION );
    if( kalypso1D2DVersion.equals( "test" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_TEST;
    else if( kalypso1D2DVersion.equals( "NEW" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
    else if( kalypso1D2DVersion.equals( "Version3.5" ) )
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
    else
      m_kalypso1D2DKernelPath = RMA10SimModelConstants.SIM_EXE_FILE_3_5;
  }

  public String getKalypso1D2DKernelPath( )
  {
    return m_kalypso1D2DKernelPath;
  }

  public Integer getIaccyc( )
  {
    Integer iaccyc = (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IACCYC );
    if( iaccyc > 1 )
    {
      m_restart = true;
    }
    m_restart = false;
    return iaccyc;
  }

  public Integer getIDNOPT( )
  {
    return (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IDNOPT );
  }

  public int getStartYear( )
  {
    return getStartCalendar().getYear();
  }

  public XMLGregorianCalendar getStartCalendar( )
  {
    return (XMLGregorianCalendar) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_startsim );
  }

  public Integer getStartJulianDay( )
  {
    GregorianCalendar calendar = getStartCalendar().toGregorianCalendar();
    return calendar.get( Calendar.DAY_OF_YEAR );
  }

  public Double getStartHour( )
  {
    return new Double( getStartCalendar().getHour() );

  }

  public Integer getIEDSW( )
  {
    return (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IEDSW );
  }

  public Double getViskosity( Feature roughnessFE )
  {
    int iedsw = ((Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IEDSW )).intValue();
    if( iedsw == 0 )
    {
      return getEddy( roughnessFE );
    }
    return getcharactV( roughnessFE );
  }

  public Double getEddy( Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_EDDY );
  }

  public Double getcharactV( Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_CHARACTV );
  }

  public Double getTBFACT( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TBFACT );
  }

  public Double getTBMIN( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TBMIN );
  }

  public Double getOMEGA( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_OMEGA );
  }

  public Double getELEV( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_ELEV );
  }

  public Double getUDIR( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_UDIR );
  }

  public Double getHMIN( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_HMIN );
  }

  public Double getDSET( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DSET );
  }

  public Double getDSETD( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DSETD );
  }

  public Integer getNITI( )
  {
    return (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NITI );
  }

  public Integer getNITN( )
  {
    return (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NITN );
  }

  public Integer getNCYC( )
  {
    return (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NCYC );
  }

  public Double getCONV_1( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_1 );
  }

  public Double getCONV_2( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_2 );
  }

  public Double getCONV_3( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_3 );
  }

  public Integer getIDRPT( )
  {
    return (Integer) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IDRPT );
  }

  public Double getDRFACT( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DRFACT );
  }

  public boolean getRestart( )
  {
    return m_restart;
  }

  public boolean getVegeta( )
  {
    return (Boolean) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VEGETA );
  }

  public Double getAC1( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC1 );
  }

  public Double getAC2( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC2 );
  }

  public Double getAC3( )
  {
    return (Double) m_controlRoot.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC3 );
  }

  public String getName( Feature feature )
  {
    return (String) feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME );
  }

  public List getRoughnessClassList( )
  {
    // return (List) m_roughnessRoot.getProperty( KalypsoModelRoughnessConsts.WBR_F_ROUGHNESS );
    return (List) m_roughnessRoot.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_ROUGHNESS_CLS_MEMBER );
  }

  public Double getKs( Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_KS );
  }

  public Double getAxAy( Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_AXAY );
  }

  public Object getDp( Feature roughnessFE )
  {
    return (Double) roughnessFE.getProperty( KalypsoModelRoughnessConsts.WBR_PROP_DP );
  }

  public List getContinuityLineList( )
  {
    // TODO Serch for BoundaryConditions (operational model) which fits to ContinuityLines (discretisation model)
    return null;
  }
}
