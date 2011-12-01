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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.Date;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.wizardPageZmlImportWithPreview.ZmlChooserControl;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.SensorException;

/**
 * Constructs a simple timeserie with a time column and a value column.
 * 
 * @author Gernot Belger
 */
public class ZmlChooserStepDescriptor implements IBoundaryConditionDescriptor
{
  protected WizardPage m_page;

  private final String m_name;

  private final ZmlChooserControl m_wizardPageZmlChooser;

  public ZmlChooserStepDescriptor( final String name, final IFolder importFolder )
  {
    m_name = name;

    m_wizardPageZmlChooser = new ZmlChooserControl( importFolder )
    {
      @Override
      protected void setComplete( final boolean complete )
      {
        m_page.setPageComplete( complete );
      }
    };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.jface.wizard.WizardPage)
   */
  @Override
  public Control createControl( final Composite parent, final WizardPage page )
  {
    m_page = page;

    return m_wizardPageZmlChooser.createControl( parent );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#activate()
   */
  @Override
  public void activate( )
  {
    m_page.setTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ZmlChooserStepDescriptor.0") ); //$NON-NLS-1$
    m_page.setDescription( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ZmlChooserStepDescriptor.1") ); //$NON-NLS-1$
    m_page.setPageComplete( false );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#fillObservation(org.kalypso.observation.IObservation,
   *      java.lang.String, java.lang.String)
   */
  @Override
  public void fillObservation( final IObservation<TupleResult> obs ) throws InvocationTargetException
  {
    final TupleResult result = obs.getResult();

    obs.setName( getName() );
    // TODO: change?
    obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) ); //$NON-NLS-1$

    // TODO: Refaktor in order to let different types of observations to be created
    final IComponent[] components = result.getComponents();

    final IComponent domainComponent = components[0];
    result.setSortComponents( new IComponent[] { domainComponent } );

    try
    {
      final org.kalypso.ogc.sensor.IObservation zmlObservation = m_wizardPageZmlChooser.getObservation();
      final ITupleModel model = zmlObservation.getValues( null );

      IAxis dateAxis;
      IAxis valueAxis;
      // System.out.println(model.getAxisList()[0].getDataClass());
      // System.out.println(model.getAxisList()[1].getDataClass());
      // TODO: this is dangerous, there is a utility class to find the axes
      if( model.getAxes()[0].getDataClass().equals( Date.class ) )
      {
        dateAxis = model.getAxes()[0];
        valueAxis = model.getAxes()[1];
      }
      else
      {
        dateAxis = model.getAxes()[1];
        valueAxis = model.getAxes()[0];
      }

      final Date fromDate = m_wizardPageZmlChooser.getFromDate();
      final Date toDate = m_wizardPageZmlChooser.getToDate();

      for( int cntFrom = 0; cntFrom < model.size(); cntFrom++ )
      {
        final Date date = (Date) model.get( cntFrom, dateAxis );

        // date must be inside the interval [fromDate, toDate]
        if( !date.before( fromDate ) && !date.after( toDate ) )
        {
          final Double doubleValue = (Double) model.get( cntFrom, valueAxis );
          final BigDecimal value = BigDecimal.valueOf( doubleValue );

          final IRecord record = result.createRecord();
          record.setValue( 0, DateUtilities.toXMLGregorianCalendar( date ) );
          record.setValue( 1, value );
          result.add( record );
        }
      }
    }
    catch( final SensorException e )
    {
      throw new InvocationTargetException( e );
    }
    // TODO
    // - get time intervall
    // - get source-uri

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#getName()
   */
  @Override
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getDomainComponentUrn()
   */
  @Override
  public String getDomainComponentUrn( )
  {
    return m_wizardPageZmlChooser.getDomainComponentUrn();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getValueComponentUrn()
   */
  @Override
  public String getValueComponentUrn( )
  {
    return m_wizardPageZmlChooser.getValueComponentUrn();
  }

}
