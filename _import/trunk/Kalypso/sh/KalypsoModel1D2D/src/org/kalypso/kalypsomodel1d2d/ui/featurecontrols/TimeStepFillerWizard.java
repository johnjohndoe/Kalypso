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
package org.kalypso.kalypsomodel1d2d.ui.featurecontrols;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypsodeegree.model.feature.Feature;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author madanago
 *
 */
public class TimeStepFillerWizard extends Wizard implements INewWizard
{
  protected static final DateFormat DF = new SimpleDateFormat( "'Manuell erzeugt am: 'dd.MM.yyyy H:mm" );
  private IStructuredSelection initialSelection;
  private TimeStepFillerWizardPage timePage;
  private Feature t_feature;
  private IObservation observation_sensor;
  private org.kalypso.observation.IObservation<TupleResult> obs;
  private IBoundaryCondition m_boundaryCondition;
  private ITuppleModel model;
  private FeatureChange[] m_changes;



  public TimeStepFillerWizard( Feature feature )
  {
    this.t_feature = feature;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    
    final Feature newFeature = t_feature;
    
    System.out.println(timePage.getStartDate());
    System.out.println(timePage.getFinishDate());
    System.out.println(timePage.getTimeSteps());
   
//    final IBoundaryCondition bc = (IBoundaryCondition) newFeature.getAdapter( IBoundaryCondition.class );
//    bc.setName( "1D-Randbedingung" ); // TODO: unterscheide zwischen verschiedenen Typen
//    bc.setDescription( DF.format( new Date() ) );

    
    //observation_sensor = (IObservation)t_feature.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );
    //observation_sensor = ObservationFeatureFactory.toObservation( t_feature );
    obs = ObservationFeatureFactory.toObservation( t_feature );

//    final String[] componentUrns = getComponentUrns();
//    final IComponent[] components = new IComponent[componentUrns.length];
////
//    for( int i = 0; i < components.length; i++ )
//      components[i] = ObservationFeatureFactory.createDictionaryComponent( t_feature, componentUrns[i] );
    
// +++++ DOUBT
    //observation = (IObservation)newFeature.getProperty(Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER);
//    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
//    obs.setName( choosenDesc.getName() );
//    obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) );

    final TupleResult result =  obs.getResult();
    final IComponent[] components = result.getComponents();
    
//    for( final IComponent component : components )
//      result.addComponent( component );
//
//    // TODO: Refaktor in order to let different types of observations to be created
    final IComponent timeComponent = components[0];
    final IComponent _underRelaxFactorComponent = components[1];
    final IComponent _QComponent = components[2];

    GregorianCalendar calendarFrom = new GregorianCalendar();
    GregorianCalendar calendarTo = new GregorianCalendar();
    //BigDecimal value = null;
//    if( !m_page1.isChoiceTimeseries() )
//    {
//      value = new BigDecimal( m_page2.getDefaultValue() );
//      calendarFrom.setTime( m_page2.getFromDate() );
//      calendarTo.setTime( m_page2.getToDate() );
//      do
//      {
//        final IRecord record = result.createRecord();
//        record.setValue( domainComponent, new XMLGregorianCalendarImpl( calendarFrom ) );
//        record.setValue( valueComponent, value );
//        result.add( record );
//        calendarFrom.add( Calendar.MINUTE, m_page2.getStep() );
//      }
//      while( !calendarFrom.after( calendarTo ) );
//    }
//    else
//    {

//    Date dd = timePage.getStartDate();
//      
//        while (dd != timePage.getFinishDate()) {
//          final IRecord record = result.createRecord();
//          GregorianCalendar calendar = new GregorianCalendar();
//          calendar.setTime();
//          record.setValue( timeComponent, new XMLGregorianCalendarImpl( calendar ) );
//          //record.setValue( _HComponent, value );
//          record.setValue( _HComponent, timePage.getHValue() );
//          record.setValue( _QComponent, timePage.getQValue() );
//          
//          result.add( record );
//        }
      
   // value = new BigDecimal( 0.0);
    calendarFrom.setTime( timePage.getStartDate() );
    calendarTo.setTime( timePage.getFinishDate() );
    List<IRecord> records = new ArrayList<IRecord>();
    while( !calendarFrom.after( calendarTo ))
    {
      final IRecord record = result.createRecord();
      record.setValue( timeComponent, new XMLGregorianCalendarImpl( calendarFrom ) );
      record.setValue( _underRelaxFactorComponent, new BigDecimal( timePage.getUnderRelaxationFactorValue()) );
      //record.setValue( _QComponent, new BigDecimal( timePage.getQValue() ) );
      result.add( record );
      //System.out.println("record :"+calendarFrom.getTimeInMillis()+" H :"+timePage.getHValue()+" Q:"+timePage.getQValue()  );
      System.out.println(record.toString());
      calendarFrom.add( Calendar.MINUTE, timePage.getTimeSteps() );
      records.add( record ); 
     
    }
    
    result.fireRecordsChanged( records.toArray( new IRecord[] {} ), ITupleResultChangedListener.TYPE.ADDED );
    m_changes = ObservationFeatureFactory.toFeatureAsChanges(  obs, t_feature );
    return true;
  }

  

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    initialSelection = selection;
    
  }
  
  @Override
  public void addPages() {
    setWindowTitle("Extract");
    timePage = new TimeStepFillerWizardPage();
    addPage(timePage);
    timePage.init(initialSelection);
 }
  
//  public String[] getComponentUrns() {
//    String[] res = new String[3];
//    res[0] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time";
//    res[1] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel";
//    res[2] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge";
////    String type = observation.getAxisList()[1].getType();
////    if(type.equals( "W" ))
////      res[1] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel";
////    if(type.equals( "Q" ))
////      res[1] = "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge";
////    return res;
////    <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time"/>
////    <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel"/>
////    <swe:component xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge"/>
//    return res;
//  }

  public FeatureChange[] getFeatureChange( )
  {
    return m_changes;
  }
 
  
  
}
