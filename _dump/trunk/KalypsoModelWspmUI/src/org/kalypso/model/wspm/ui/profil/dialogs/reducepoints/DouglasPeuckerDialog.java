/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.ui.profil.dialogs.reducepoints;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.IOperationHistory;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.util.DouglasPeuckerHelper;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationRunnable;
import org.kalypso.observation.result.IRecord;

/**
 * @author Belger
 */
public class DouglasPeuckerDialog extends TitleAreaDialog
{
  private static final String SETTINGS_SECTION = "douglasPeuckerDialogSettings"; //$NON-NLS-1$

  private static final String SETTINGS_WIDTH = "width"; //$NON-NLS-1$

  private static final String SETTINGS_HEIGHT = "height"; //$NON-NLS-1$

  private static final String SETTINGS_X = "posx"; //$NON-NLS-1$

  private static final String SETTINGS_Y = "posy"; //$NON-NLS-1$

  private static final String SETTINGS_DISTANCE = "distance"; //$NON-NLS-1$

  private static final String SETTINGS_PROVIDER = null;

  private final IPointsProvider[] m_pointsProviders;

  private final IProfil m_profile;

  private IPointsProvider m_provider = null;

  protected double m_distance = Double.NaN;

  private ProfilOperation m_operation = null;

  private IDialogSettings m_dialogSettings;

  public DouglasPeuckerDialog( final Shell parentShell, final IProfil profile, final IPointsProvider[] pointsProviders )
  {
    super( parentShell );

    m_profile = profile;
    m_pointsProviders = pointsProviders;
    final IDialogSettings dialogSettings = KalypsoModelWspmUIPlugin.getDefault().getDialogSettings();
    m_dialogSettings = dialogSettings.getSection( SETTINGS_SECTION );
    if( m_dialogSettings == null )
      m_dialogSettings = dialogSettings.addNewSection( SETTINGS_SECTION );

    if( m_dialogSettings.get( SETTINGS_WIDTH ) == null )
      m_dialogSettings.put( SETTINGS_WIDTH, 0 );

    if( m_dialogSettings.get( SETTINGS_HEIGHT ) == null )
      m_dialogSettings.put( SETTINGS_HEIGHT, 0 );

    if( m_dialogSettings.get( SETTINGS_DISTANCE ) == null )
      m_dialogSettings.put( SETTINGS_DISTANCE, 0.5 );

    if( m_dialogSettings.get( SETTINGS_X ) == null )
      m_dialogSettings.put( SETTINGS_X, -1 );

    if( m_dialogSettings.get( SETTINGS_Y ) == null )
      m_dialogSettings.put( SETTINGS_Y, -1 );

    setShellStyle( getShellStyle() | SWT.RESIZE );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#create()
   */
  @Override
  public void create( )
  {
    super.create();

    getShell().setText( Messages.DouglasPeuckerDialog_6 );
    setTitle( Messages.DouglasPeuckerDialog_7 );

    updateDialog();
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#close()
   */
  @Override
  public boolean close( )
  {
    final Shell shell = getShell();
    if( shell == null || shell.isDisposed() )
      return true;

    IStatus status = null;
    try
    {
      // if Dialog was cancelled, reset state of profile
      if( getReturnCode() == CANCEL )
        status = resetState();
    }
    catch( final Throwable e )
    {
      status = StatusUtilities.statusFromThrowable( e );
    }

    if( status != null )
      ErrorDialog.openError( shell, shell.getText(), Messages.DouglasPeuckerDialog_8, status );

    // save dialog settings
    final Point size = shell.getSize();
    final Point location = shell.getLocation();
    m_dialogSettings.put( SETTINGS_WIDTH, size.x );
    m_dialogSettings.put( SETTINGS_HEIGHT, size.y );
    m_dialogSettings.put( SETTINGS_X, location.x );
    m_dialogSettings.put( SETTINGS_Y, location.y );
    m_dialogSettings.put( SETTINGS_PROVIDER, m_provider == null ? null : m_provider.getName() );

    if( !Double.isNaN( m_distance ) )
      m_dialogSettings.put( SETTINGS_DISTANCE, m_distance );

    return super.close();
  }

  /**
   * @see org.eclipse.jface.window.Window#getInitialLocation(org.eclipse.swt.graphics.Point)
   */
  @Override
  protected Point getInitialLocation( final Point initialSize )
  {
    final int x = m_dialogSettings.getInt( SETTINGS_X );
    final int y = m_dialogSettings.getInt( SETTINGS_Y );

    if( x == -1 && y == -1 )
      return super.getInitialLocation( initialSize );

    return new Point( x, y );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#getInitialSize()
   */
  @Override
  protected Point getInitialSize( )
  {
    final Point defaultSize = super.getInitialSize();

    final int lastWidth = m_dialogSettings.getInt( SETTINGS_WIDTH );
    final int lastHeight = m_dialogSettings.getInt( SETTINGS_HEIGHT );

    // HACK: the calculated width is always too big, so we reduce it a bit
    return new Point( Math.max( defaultSize.x, lastWidth ), Math.max( defaultSize.y - 130, lastHeight ) );
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final FormToolkit toolkit = new FormToolkit( parent.getDisplay() );

    // create the top level composite for the dialog area
    final Composite composite = toolkit.createComposite( parent );
    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    final GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.verticalSpacing = 0;
    layout.horizontalSpacing = 0;
    composite.setLayout( layout );

    // Build the separator line
    final Label titleBarSeparator = toolkit.createSeparator( composite, SWT.HORIZONTAL | SWT.SEPARATOR );
    titleBarSeparator.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Composite groupComposite = toolkit.createComposite( composite, SWT.NONE );
    final GridLayout groupLayout = new GridLayout();
    groupLayout.marginHeight = 10;
    groupLayout.marginWidth = 10;
    groupLayout.verticalSpacing = 10;
    groupLayout.horizontalSpacing = 10;
    groupComposite.setLayout( groupLayout );
    groupComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    createHelpArea( toolkit, groupComposite ).setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    createPointProviderGroup( toolkit, groupComposite ).setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    createDistanceGroup( toolkit, groupComposite ).setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    // Build the separator line
    final Label buttonBarSeparator = toolkit.createSeparator( composite, SWT.HORIZONTAL | SWT.SEPARATOR );
    buttonBarSeparator.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    return composite;
  }

  private Control createHelpArea( final FormToolkit toolkit, final Composite groupComposite )
  {
    final Label label = toolkit.createLabel( groupComposite, Messages.DouglasPeuckerDialog_9, SWT.WRAP );
    return label;
  }

  private Composite createPointProviderGroup( final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.DESCRIPTION | Section.TITLE_BAR | Section.EXPANDED );

    section.setLayout( new GridLayout( 1, false ) );
    section.setText( Messages.DouglasPeuckerDialog_10 );
    section.setDescription( Messages.DouglasPeuckerDialog_11 );

    final Composite sectionClient = toolkit.createComposite( section );
    sectionClient.setLayout( new GridLayout() );
    section.setClient( sectionClient );

    Button buttonToSelect = null;
    IPointsProvider providerToSelect = null;

    for( final IPointsProvider provider : m_pointsProviders )
    {
      final Button button = toolkit.createButton( sectionClient, provider.getName(), SWT.RADIO );

      button.addSelectionListener( new SelectionListener()
      {
        public void widgetSelected( final SelectionEvent e )
        {
          handleRadioSelected( provider );
        }

        public void widgetDefaultSelected( final SelectionEvent e )
        {
          handleRadioSelected( provider );
        }
      } );

      final String lastProvider = m_dialogSettings.get( SETTINGS_PROVIDER );
      provider.getName();

      if( provider.getName().equals( lastProvider ) || provider == m_pointsProviders[0] )
      {
        buttonToSelect = button;
        providerToSelect = provider;
      }

    }

    buttonToSelect.setSelection( true );
    m_provider = providerToSelect;

    return section;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  private Composite createDistanceGroup( final FormToolkit toolkit, final Composite parent )
  {
    final Section section = toolkit.createSection( parent, Section.DESCRIPTION | Section.TITLE_BAR | Section.EXPANDED );

    section.setLayout( new GridLayout( 2, false ) );
    section.setText( Messages.DouglasPeuckerDialog_12 );
    section.setDescription( Messages.DouglasPeuckerDialog_13 );

    final Composite sectionClient = toolkit.createComposite( section );
    sectionClient.setLayout( new GridLayout( 2, false ) );
    section.setClient( sectionClient );

    toolkit.createLabel( section, Messages.DouglasPeuckerDialog_14, SWT.NONE );

    final String lastDistanceStr = m_dialogSettings.get( SETTINGS_DISTANCE );
    m_distance = NumberUtils.parseQuietDouble( lastDistanceStr );

    final Spinner spinner = new Spinner( sectionClient, SWT.NONE );
    toolkit.adapt( spinner, true, true );
    spinner.setData( FormToolkit.KEY_DRAW_BORDER, FormToolkit.TEXT_BORDER );

    final GridData gridData = new GridData();
    gridData.minimumWidth = 50;
    gridData.widthHint = 50;
    spinner.setLayoutData( gridData );
    spinner.setDigits( 2 );
    spinner.setMaximum( Integer.MAX_VALUE );
    spinner.setIncrement( 10 );
    spinner.setPageIncrement( 100 );
    spinner.setSelection( (int) (m_distance * 100) );
    spinner.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        handleDistanceChanged( spinner.getSelection() / 100.0 );
      }
    } );

    toolkit.createLabel( sectionClient, Messages.DouglasPeuckerDialog_15, SWT.LEFT );

    toolkit.paintBordersFor( sectionClient );

    return section;
  }

  protected void handleRadioSelected( final IPointsProvider provider )
  {
    m_provider = provider;

    updateDialog();
  }

  protected void handleDistanceChanged( final double distance )
  {
    m_distance = distance;

    updateDialog();
  }

  protected void updateDialog( )
  {
    // reset
    try
    {
      final IStatus status = resetState();
      if( !status.isOK() )
      {
        setErrorMessage( status.getMessage() );
        return;
      }
    }
    catch( final ExecutionException e )
    {
      e.printStackTrace();

      setErrorMessage( e.getLocalizedMessage() );
      return;
    }

    // check controls
    final String errorMessage = m_provider == null ? null : m_provider.getErrorMessage();
    if( errorMessage != null )
    {
      setErrorMessage( errorMessage );
      return;
    }

    if( Double.isNaN( m_distance ) )
    {
      setErrorMessage( Messages.DouglasPeuckerDialog_16 );
      return;
    }

    // ausd�nn again
    final IStatus status = performReduce();
    if( !status.isOK() )
      setErrorMessage( status.getMessage() );
    else
    {
      setErrorMessage( null );
      setMessage( status.getMessage() );
    }
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#setErrorMessage(java.lang.String)
   */
  @Override
  public void setErrorMessage( final String newErrorMessage )
  {
    super.setErrorMessage( newErrorMessage );

    final Button button = getButton( IDialogConstants.OK_ID );
    if( button != null )
      button.setEnabled( newErrorMessage == null );
  }

  private IStatus performReduce( )
  {
    if( m_provider == null || Double.isNaN( m_distance ) )
      return StatusUtilities.createWarningStatus( Messages.DouglasPeuckerDialog_17 );

    /* Get important values. */
    final double allowedDistance = m_distance;
    final IRecord[] points = m_provider.getPoints();

    /* Get the profile changes. */
    IProfilChange[] removeChanges = DouglasPeuckerHelper.reduce( allowedDistance, points, m_profile );
    if( removeChanges.length == 0 )
      return StatusUtilities.createOkStatus( Messages.DouglasPeuckerDialog_18 );

    /* Create the profile operation. */
    m_operation = new ProfilOperation( Messages.DouglasPeuckerDialog_19, m_profile, removeChanges, false );

    /* Create the runnable. */
    final ProfilOperationRunnable operationRunnable = new ProfilOperationRunnable( m_operation );

    /* Execute the value. */
    final IStatus operationStatus = operationRunnable.execute( new NullProgressMonitor() );
    if( !operationStatus.isOK() )
      return operationStatus;

    /* Message for the user. */
    final String message = Messages.DouglasPeuckerDialog_20 + removeChanges.length + Messages.DouglasPeuckerDialog_21 + points.length + Messages.DouglasPeuckerDialog_22;

    return StatusUtilities.createOkStatus( message );
  }

  private IStatus resetState( ) throws ExecutionException
  {
    if( m_operation != null )
    {
      final IOperationHistory operationHistory = PlatformUI.getWorkbench().getOperationSupport().getOperationHistory();
      final IStatus status = operationHistory.undoOperation( m_operation, new NullProgressMonitor(), null );
      m_operation = null;
      return status;
    }

    return Status.OK_STATUS;
  }
}