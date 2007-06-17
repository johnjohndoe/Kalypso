/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.math.LinearEquation;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogPageUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.Phenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * Constructs a simple timeserie with a time columnd and a value column.
 * 
 * @author Gernot Belger
 */
public class WQStepDescriptor implements IBoundaryConditionDescriptor
{
  private static final String MSG_PAGE = "Auf dieser Seite kann die gewünschte W/Q-Beziehung definiert werden.";

  private static final String DEFAULTSTEP = "10";

  private WizardPage m_page;

  private final String m_domainComponentUrn;

  private final String m_valueComponentUrn;

  private final String m_name;

  private final Double[] m_h = new Double[2];

  private final Double[] m_q = new Double[2];

  private Integer m_step;

  public WQStepDescriptor( final String name )
  {
    m_name = name;

    m_domainComponentUrn = Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL;
    m_valueComponentUrn = Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.jface.wizard.WizardPage)
   */
  public Control createControl( final Composite parent, final WizardPage page )
  {
    m_page = page;

    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout( 3, false );
    container.setLayout( gridLayout );

    new Label( container, SWT.NONE );
    // TODO: check needed units
    new Label( container, SWT.NONE ).setText( "h [cm]" );
    new Label( container, SWT.NONE ).setText( "Q [m³/s]" );

    createFromToLine( container, "Von:", 0, 0.0, 0.0 );
    createFromToLine( container, "Bis:", 1, 100.0, 10.0 );

    final Label stepLabel = new Label( container, SWT.NONE );
    stepLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    stepLabel.setText( "Anzahl Schritte:" );

    final Text stepText = new Text( container, SWT.BORDER );
    stepText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stepText.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = stepText.getText();
        handleStepChanged( text );
      }
    } );
    stepText.setText( DEFAULTSTEP );

    new Label( container, SWT.NONE );

    updatePageState( Status.OK_STATUS );

    return container;
  }

  private void createFromToLine( final Composite parent, final String label, final int index, final double h, final double q )
  {
    final Label labelLabel = new Label( parent, SWT.NONE );
    labelLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    labelLabel.setText( label );

    final Text textH = new Text( parent, SWT.BORDER );
    textH.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    textH.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = textH.getText();
        handleHChanged( index, text );
      }
    } );

    textH.setText( String.format( "%.0", h ) );

    final Text textQ = new Text( parent, SWT.BORDER );
    textQ.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    textQ.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        final String text = textQ.getText();
        handleQChanged( index, text );
      }
    } );

    textQ.setText( String.format( "%.3", q ) );
  }

  protected void updatePageState( final IStatus status )
  {
    final IStatus pageStatus = status.isOK() ? checkPageComplete() : status;
    if( pageStatus.isOK() )
      m_page.setMessage( MSG_PAGE, DialogPage.NONE );
    else
      m_page.setMessage( pageStatus.getMessage(), DialogPageUtilitites.severityToMessagecode( pageStatus ) );

    m_page.setPageComplete( pageStatus.isOK() );
  }

  private IStatus checkPageComplete( )
  {
    if( m_h[0] == null )
      return StatusUtilities.createWarningStatus( "Geben Sie einen unteren Wasserstand ein." );
    if( m_h[1] == null )
      return StatusUtilities.createWarningStatus( "Geben Sie einen oberen Wasserstand ein." );
    if( m_q[0] == null )
      return StatusUtilities.createWarningStatus( "Geben Sie ein unteren Abfluss ein." );
    if( m_q[1] == null )
      return StatusUtilities.createWarningStatus( "Geben Sie ein oberen Abfluss ein." );
    if( m_step == null )
      return StatusUtilities.createWarningStatus( "Geben Sie eine Schrittanahl ein." );

    if( !(m_h[0] < m_h[1]) )
      return StatusUtilities.createErrorStatus( "Unterer Wasserstand muss kleiner als der obere Wasserstand sein." );
    if( !(m_q[0] < m_q[1]) )
      return StatusUtilities.createErrorStatus( "Unterer Abfluss muss kleiner als der obere Abfluss sein." );

    return Status.OK_STATUS;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#activate()
   */
  public void activate( )
  {
    m_page.setTitle( "W/Q-Beziehung definieren" );
    m_page.setDescription( MSG_PAGE );

    updatePageState( Status.OK_STATUS );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#initializeBC(org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition)
   */
  public void initializeBC( final IBoundaryCondition bc )
  {
    bc.setName( getName() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#fillObservation(org.kalypso.observation.IObservation)
   */
  public void fillObservation( final IObservation<TupleResult> obs ) throws InvocationTargetException
  {
    try
    {
      final TupleResult result = obs.getResult();

      obs.setName( getName() );
      // TODO: change
      obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) );

      final IComponent[] components = result.getComponents();
      final IComponent domainComponent = components[0];
      final IComponent valueComponent = components[1];
      result.setSortComponents( new IComponent[] { domainComponent } );

      final Double hStop = m_h[1];
      final Double hStart = m_h[0];
      final LinearEquation linearEquation = new LinearEquation( hStart, m_q[0], hStart, hStop );

      for( double h = hStart; h < hStop; h += ((hStop - hStart) / m_step) )
      {
        final double q = linearEquation.computeY( h );

        final IRecord record = result.createRecord();
        record.setValue( domainComponent, new BigDecimal( h ).setScale( 1, BigDecimal.ROUND_HALF_UP ) );
        record.setValue( valueComponent, new BigDecimal( q ).setScale( 3, BigDecimal.ROUND_HALF_UP ) );
        result.add( record );
      }
    }
    catch( final Throwable t )
    {
      throw new InvocationTargetException( t );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ITimeserieTypeDescriptor#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  protected void handleStepChanged( final String text ) throws NumberFormatException
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_step = new Integer( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, "Formatfehler im Zeitschritt: geben Sie einen Ganzzahlwert ein" );
      m_step = null;
    }

    updatePageState( status );
  }

  protected void handleHChanged( final int index, final String text )
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_h[index] = new Double( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, "Formatfehler in Wasserstand. Geben Sie eine Dezimalzahl ein." );

      m_h[index] = null;
    }

    updatePageState( status );
  }

  protected void handleQChanged( final int index, final String text )
  {
    IStatus status = Status.OK_STATUS;
    try
    {
      m_q[index] = new Double( text );
    }
    catch( final Throwable t )
    {
      status = StatusUtilities.statusFromThrowable( t, "Formatfehler in Wasserstand. Geben Sie eine Dezimalzahl ein." );

      m_q[index] = null;
    }

    updatePageState( status );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getDomainComponentUrn()
   */
  public String getDomainComponentUrn( )
  {
    return m_domainComponentUrn;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getValueComponentUrn()
   */
  public String getValueComponentUrn( )
  {
    return m_valueComponentUrn;
  }
}
