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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

import javax.xml.datatype.XMLGregorianCalendar;

import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author madanago
 */
public class TimeStepFillerWizard extends Wizard
{
  private TimeStepFillerWizardPage m_timeStepFillerWizardPage;

  private final Feature m_feature;

  private final IObservation<TupleResult> m_observation;

  private FeatureChange[] m_changes;

  private final TupleResult m_result;

  public TimeStepFillerWizard( final Feature feature )
  {
    m_feature = feature;
    m_observation = ObservationFeatureFactory.toObservation( m_feature );
    m_result = m_observation.getResult();
  }

  @Override
  public boolean performFinish( )
  {
    // clear currently present result list
    m_result.clear();
    // get components of time step definition list
    final IComponent[] components = m_result.getComponents();
    final IComponent ordinalNumberComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER );
    final IComponent timeComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final IComponent relaxFactorComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR );

    // get record list of new defined time steps
    final List<IRecord> records = new ArrayList<>();
    final GregorianCalendar calendarFrom = new GregorianCalendar();
    final GregorianCalendar calendarTo = new GregorianCalendar();
    calendarFrom.setTime( m_timeStepFillerWizardPage.getStartDate() );
    calendarTo.setTime( m_timeStepFillerWizardPage.getFinishDate() );
    calendarFrom.setTimeZone( TimeZone.getTimeZone( "UTC" ) ); //$NON-NLS-1$
    calendarTo.setTimeZone( TimeZone.getTimeZone( "UTC" ) ); //$NON-NLS-1$

    int ordinalNumber = 1;
    while( !calendarFrom.after( calendarTo ) )
    {
      final IRecord record = m_result.createRecord();
      record.setValue( m_result.indexOfComponent( ordinalNumberComponent ), new BigInteger( Integer.toString( ordinalNumber++ ) ) );
      record.setValue( m_result.indexOfComponent( timeComponent ), DateUtilities.toXMLGregorianCalendar( calendarFrom ) );
      record.setValue( m_result.indexOfComponent( relaxFactorComponent ), m_timeStepFillerWizardPage.getUnderRelaxationFactorValue() );
      calendarFrom.add( m_timeStepFillerWizardPage.getField(), m_timeStepFillerWizardPage.getTimeSteps() );
      records.add( record );
    }
    m_result.addAll( records );
    m_changes = ObservationFeatureFactory.toFeatureAsChanges( m_observation, m_feature );
    return true;
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.featurecontrols.TimeStepFillerWizard.2" ) ); //$NON-NLS-1$

    if( m_result.size() == 0 )
      m_timeStepFillerWizardPage = new TimeStepFillerWizardPage();
    else
    {
      final IComponent[] components = m_result.getComponents();
      final IComponent timeComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
      final int indexOfTimeComponent = m_result.indexOfComponent( timeComponent );
      final IComponent uRelComponent = ComponentUtilities.findComponentByID( components, Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR );
      final int indexOfURelComponent = m_result.indexOfComponent( uRelComponent );

      final IRecord startRecord = m_result.get( 0 );
      final IRecord endRecord = m_result.get( m_result.size() - 1 );

      final Date startDate = DateUtilities.toDate( (XMLGregorianCalendar) startRecord.getValue( indexOfTimeComponent ) );
      final Date endDate = DateUtilities.toDate( (XMLGregorianCalendar) endRecord.getValue( indexOfTimeComponent ) );
      // changed to string to allow more flexible expansion of "Relaxation Factor"
      final String uRelFactor = (String) startRecord.getValue( indexOfURelComponent );
      int timeStep = 60;
      if( m_result.size() > 1 )
      {
        final IRecord secondRecord = m_result.get( 1 );
        final Date secondDate = DateUtilities.toDate( (XMLGregorianCalendar) secondRecord.getValue( indexOfTimeComponent ) );

        final long distMillis = secondDate.getTime() - startDate.getTime();
        final double distMinutes = distMillis / 1000.0 / 60.0;
        timeStep = (int) Math.round( distMinutes );
      }

      m_timeStepFillerWizardPage = new TimeStepFillerWizardPage( startDate, endDate, uRelFactor, timeStep );
    }

    addPage( m_timeStepFillerWizardPage );
  }

  public FeatureChange[] getFeatureChange( )
  {
    return m_changes;
  }

}
