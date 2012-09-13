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
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
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
  private static final String MSG_PAGE = Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.0"); //$NON-NLS-1$

  private static final String DEFAULTSTEP = "10"; //$NON-NLS-1$

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
  @Override
  public Control createControl( final Composite parent, final WizardPage page )
  {
    m_page = page;

    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout( 3, false );
    container.setLayout( gridLayout );

    new Label( container, SWT.NONE );
    // TODO: check needed units
    new Label( container, SWT.NONE ).setText( "h [cm]" ); //$NON-NLS-1$
    new Label( container, SWT.NONE ).setText( "Q [m≥/s]" ); //$NON-NLS-1$

    createFromToLine( container, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.4"), 0, 0.0, 0.0 ); //$NON-NLS-1$
    createFromToLine( container, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.5"), 1, 100.0, 10.0 ); //$NON-NLS-1$

    final Label stepLabel = new Label( container, SWT.NONE );
    stepLabel.setLayoutData( new GridData( SWT.BEGINNING, SWT.CENTER, false, false ) );
    stepLabel.setText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.6") ); //$NON-NLS-1$

    final Text stepText = new Text( container, SWT.BORDER );
    stepText.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    stepText.addModifyListener( new ModifyListener()
    {
      @Override
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
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String text = textH.getText();
        handleHChanged( index, text );
      }
    } );

    textH.setText( String.format( "%s", h ) ); //$NON-NLS-1$

    final Text textQ = new Text( parent, SWT.BORDER );
    textQ.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    textQ.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final String text = textQ.getText();
        handleQChanged( index, text );
      }
    } );

    textQ.setText( String.format( "%s", q ) ); //$NON-NLS-1$
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
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.9" ) ); //$NON-NLS-1$
    if( m_h[1] == null )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.10" ) ); //$NON-NLS-1$
    if( m_q[0] == null )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.11" ) ); //$NON-NLS-1$
    if( m_q[1] == null )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.12" ) ); //$NON-NLS-1$
    if( m_step == null )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.13" ) ); //$NON-NLS-1$

    if( !(m_h[0] < m_h[1]) )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.14" ) ); //$NON-NLS-1$
    if( !(m_q[0] < m_q[1]) )
      return new Status( IStatus.WARNING, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.15" ) ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

  @Override
  public void activate( )
  {
    m_page.setTitle( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.16") ); //$NON-NLS-1$
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
  @Override
  public void fillObservation( final IObservation<TupleResult> obs ) throws InvocationTargetException
  {
    try
    {
      final TupleResult result = obs.getResult();

      obs.setName( getName() );
      // TODO: change
      obs.setPhenomenon( new Phenomenon( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", null, null ) ); //$NON-NLS-1$

      final IComponent[] components = result.getComponents();
      final IComponent domainComponent = components[0];
      result.setSortComponents( new IComponent[] { domainComponent } );

      final Double hStart = m_h[0];
      final Double hStop = m_h[1];
      final LinearEquation linearEquation = new LinearEquation( hStart, m_q[0], hStop, m_q[1] );

      for( double h = hStart; h < hStop; h += ((hStop - hStart) / m_step) )
      {
        final double q = linearEquation.computeY( h );

        final IRecord record = result.createRecord();
        record.setValue( 0, new BigDecimal( h ).setScale( 1, BigDecimal.ROUND_HALF_UP ) );
        record.setValue( 1, new BigDecimal( q ).setScale( 3, BigDecimal.ROUND_HALF_UP ) );
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
  @Override
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
      status = StatusUtilities.statusFromThrowable( t, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.18") ); //$NON-NLS-1$
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
      status = StatusUtilities.statusFromThrowable( t, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.19") ); //$NON-NLS-1$

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
      status = StatusUtilities.statusFromThrowable( t, Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor.20") ); //$NON-NLS-1$

      m_q[index] = null;
    }

    updatePageState( status );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getDomainComponentUrn()
   */
  @Override
  public String getDomainComponentUrn( )
  {
    return m_domainComponentUrn;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor#getValueComponentUrn()
   */
  @Override
  public String getValueComponentUrn( )
  {
    return m_valueComponentUrn;
  }
}
