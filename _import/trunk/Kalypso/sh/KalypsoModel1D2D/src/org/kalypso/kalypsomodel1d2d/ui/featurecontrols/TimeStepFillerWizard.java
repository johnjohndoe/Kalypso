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
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

import com.sun.org.apache.xerces.internal.jaxp.datatype.XMLGregorianCalendarImpl;

/**
 * @author madanago
 */
public class TimeStepFillerWizard extends Wizard implements INewWizard
{
  private IStructuredSelection m_initialSelection;

  private TimeStepFillerWizardPage m_timeStepFillerWizardPage;

  private Feature m_feature;

  private IObservation<TupleResult> m_observation;

  private FeatureChange[] m_changes;

  public TimeStepFillerWizard( final Feature feature )
  {
    m_feature = feature;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    m_observation = ObservationFeatureFactory.toObservation( m_feature );
    final TupleResult result = m_observation.getResult();
    result.clear();
    final IComponent[] components = result.getComponents();
    final IComponent ordinalNumberComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER );
    final IComponent timeComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final IComponent relaxFactorComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR );
    final List<IRecord> records = new ArrayList<IRecord>();
    final GregorianCalendar calendarFrom = new GregorianCalendar();
    final GregorianCalendar calendarTo = new GregorianCalendar();
    calendarFrom.setTime( m_timeStepFillerWizardPage.getStartDate() );
    calendarTo.setTime( m_timeStepFillerWizardPage.getFinishDate() );
    int ordinalNumber = 1;
    while( !calendarFrom.after( calendarTo ) )
    {
      final IRecord record = result.createRecord();
      record.setValue( ordinalNumberComponent, new BigInteger( Integer.toString( ordinalNumber++ ) ) );
      record.setValue( timeComponent, new XMLGregorianCalendarImpl( calendarFrom ) );
      record.setValue( relaxFactorComponent, new BigDecimal( m_timeStepFillerWizardPage.getUnderRelaxationFactorValue() ).setScale( 3, BigDecimal.ROUND_HALF_UP ) );
      result.add( record );
      calendarFrom.add( Calendar.MINUTE, m_timeStepFillerWizardPage.getTimeSteps() );
      records.add( record );
    }
    result.fireRecordsChanged( records.toArray( new IRecord[] {} ), ITupleResultChangedListener.TYPE.ADDED );
    m_changes = ObservationFeatureFactory.toFeatureAsChanges( m_observation, m_feature );
    return true;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    m_initialSelection = selection;
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( "Berechnungszeitschritte definieren" );
    m_timeStepFillerWizardPage = new TimeStepFillerWizardPage();
    addPage( m_timeStepFillerWizardPage );
    m_timeStepFillerWizardPage.init( m_initialSelection );
  }

  public FeatureChange[] getFeatureChange( )
  {
    return m_changes;
  }

}
