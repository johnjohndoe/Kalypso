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
package org.kalypso.kalypsomodel1d2d.schema.binding.model;

import java.io.File;
import java.math.BigInteger;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.StringTokenizer;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.core.runtime.FileLocator;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * @author Dejan Antanaskovic
 * 
 */
@SuppressWarnings("unchecked")
public class ControlModel1D2D extends AbstractFeatureBinder implements IControlModel1D2D
{
  public ControlModel1D2D( final Feature featureToBind )
  {
    this( featureToBind, Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL );
  }

  public ControlModel1D2D( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getTimeSteps()
   */
  public IObservation<TupleResult> getTimeSteps( )
  {
    try
    {
      final Feature feature = getFeature();
      final Object property = feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
      final IObservation<TupleResult> obs = (IObservation<TupleResult>) ((Feature) property).getAdapter( IObservation.class );
      return obs;
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  public Integer getIDNOPT( )
  {
    return (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IDNOPT );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getVersion()
   */
  public String getVersion( )
  {
    return (String) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VERSION );
  }

  public int getStartYear( )
  {
    return getStartCalendar().getYear();
  }

  public Integer getIaccyc( )
  {
    final Object property = getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IACCYC );
    if( property instanceof BigInteger )
      return ((BigInteger) property).intValue();
    return (Integer) property;
  }

  public boolean getRestart( )
  {
    return ((Boolean) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART )).booleanValue();
  }

  public List<String> getRestartPaths( )
  {
    final List<String> list = new ArrayList<String>();
    final Object property = getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART_PATH );
    if( property == null )
      return list;

    // TODO: what about String.split?
    final StringTokenizer tokenizer = new StringTokenizer( property.toString(), ";" );
    while( tokenizer.hasMoreTokens() )
      list.add( tokenizer.nextToken() );
    return list;
  }

  public void setRestartPaths( final List<String> list )
  {
    if( list != null && list.size() > 0 )
    {
      final String paths = list.get( 0 );
      for( int i = 1; i < list.size(); i++ )
        paths.concat( ";" ).concat( list.get( i ) );
      getFeature().setProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART_PATH, paths );
    }
    else
      getFeature().setProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_RESTART_PATH, "" );
  }

  public XMLGregorianCalendar getStartCalendar( )
  {
    return (XMLGregorianCalendar) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_STARTSIM );
  }

  public Integer getStartJulianDay( )
  {
    final GregorianCalendar calendar = getStartCalendar().toGregorianCalendar();
    return calendar.get( Calendar.DAY_OF_YEAR );
  }

  public Double getStartHour( )
  {
    return new Double( getStartCalendar().getHour() );
  }

  public Integer getIEDSW( )
  {
    final Integer property = (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IEDSW );
    return property != null ? property : 0;
  }

  public Double getTBFACT( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TBFACT );
    return property != null ? property : 0.0;
  }

  public Double getTBMIN( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TBMIN );
    return property != null ? property : 0.0;
  }

  public Double getOMEGA( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_OMEGA );
    return property != null ? property : 0.0;
  }

  public Double getELEV( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_ELEV );
    return property != null ? property : 0.0;
  }

  public Double getUDIR( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_UDIR );
    return property != null ? property : 0.0;
  }

  public Double getUNOM( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_UNOM );
    return property != null ? property : 0.0;
  }

  public Double getHMIN( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_HMIN );
    return property != null ? property : 0.0;
  }

  public Double getDSET( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DSET );
    return property != null ? property : 0.0;
  }

  public Double getDSETD( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DSETD );
    return property != null ? property : 0.0;
  }

  public Integer getNITI( )
  {
    Integer property = (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NITI );
    if( property == null )
      property = 0;
    return property;
  }

  public Integer getNITN( )
  {
    Integer property = (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_NITN );
    if( property == null )
      property = 0;
    return property;
  }

  public Integer getNCYC( )
  {
    final Integer property = getTimeSteps().getResult().size() - 1;
    return getNITN() != null ? property : 0; // Not needed, while no definition of unsteady timesteps
  }

  public Double getCONV_1( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_1 );
    return property != null ? property : 0.0;
  }

  public Double getCONV_2( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_2 );
    return property != null ? property : 0.0;
  }

  public Double getCONV_3( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONV_3 );
    return property != null ? property : 0.0;
  }

  public Integer getIDRPT( )
  {
    final Integer property = (Integer) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_IDRPT );
    return property != null ? property : 0;
  }

  public Double getDRFACT( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_DRFACT );
    return property != null ? property : 0.0;
  }

  public boolean getVegeta( )
  {
    return (Boolean) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_VEGETA );
  }

  public Double getAC1( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC1 );
    return property != null ? property : 0.0;
  }

  public Double getAC2( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC2 );
    return property != null ? property : 0.0;
  }

  public Double getAC3( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_AC3 );
    return property != null ? property : 0.0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#setCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  public void setCalculationUnit( final ICalculationUnit calUnit )
  {
    try
    {
      final Feature wrappedFeature = calUnit.getWrappedFeature();
      final Feature parentFeature = getFeature();// control to link to
      final IPropertyType property = parentFeature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT );

      final GMLWorkspace workspace = calUnit.getWrappedFeature().getWorkspace();
      final URL context = workspace.getContext();
      final File filee = new File( FileLocator.resolve( context ).getFile() );
      final String name = filee.getName();// new java.io.File( uri ).getName();
      final XLinkedFeature_Impl linkedFeature = (XLinkedFeature_Impl) FeatureHelper.createLinkToID( name + "#" + calUnit.getGmlID(), parentFeature, (IRelationType) property, wrappedFeature.getFeatureType() );
      parentFeature.setProperty( property, linkedFeature );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( "Exception while setting calunit link to control", e );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#getCalculationUnit()
   */
  public ICalculationUnit getCalculationUnit( )
  {
    try
    {
      final ICalculationUnit resolveLink = FeatureHelper.resolveLink( this, Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT, ICalculationUnit.class );
      return resolveLink;
    }
    catch( final IllegalStateException e )
    {
      // appends if the gml are not saved yet
      // hacking by getting the feature id and returning
      // the one id the control workspace with the same id
      // this will only work in the same scenario context
      final Feature resolveLink = FeatureHelper.resolveLink( this.getFeature(), Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT );
      if( !(resolveLink instanceof XLinkedFeature_Impl) )
      {
        throw e;
      }
      final String featureId = ((XLinkedFeature_Impl) resolveLink).getFeatureId();
      final IFEDiscretisationModel1d2d model = Util.getModel( IFEDiscretisationModel1d2d.class );
      if( model == null )
      {
        throw e;
      }
      final GMLWorkspace modelWorkspace = model.getWrappedFeature().getWorkspace();
      final Feature calUnitFeature = modelWorkspace.getFeature( featureId );
      if( calUnitFeature == null )
      {
        return null;
      }
      final ICalculationUnit adapter = (ICalculationUnit) calUnitFeature.getAdapter( ICalculationUnit.class );
      return adapter;
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#get_p_bottom()
   */
  public Double get_P_BOTTOM( )
  {
    final Double property = (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_P_BOTTOM );
    return property != null ? property : 0.0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D#get_steadyBC()
   */
  public Double get_steadyBC( )
  {
    return (Double) getFeature().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_STEADY_BC );
  }

}
