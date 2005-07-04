/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.contribs.eclipse.jface.wizard.view;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.FontMetrics;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IProgressService;

/**
 * A {@link org.eclipse.ui.IViewPart}which is a wizard container.
 * <p>
 * The {@link org.eclipse.jface.wizard.IWizard}must be set from the outside.
 * </p>
 * <p>
 * Lots of the code was taken from {@link org.eclipse.jface.wizard.WizardDialog}.
 * </p>
 * 
 * @author belger
 */
public class WizardView extends ViewPart implements IWizardContainer3
{
  private final List m_listeners = new ArrayList( 5 );

  private IWizard m_wizard;

  private IWizardPage m_currentPage;

  private Composite m_pageContainer;

  private static final StackLayout m_stackLayout = new StackLayout();

  private Composite m_panel;

  private final Map m_buttons = new HashMap( 10 );

  private FontMetrics m_fontMetrics;

  /** Sets an new wizard and immediately displays its first page */
  public void setWizard( final IWizard wizard )
  {
    disposeWizard();

    m_wizard = wizard;
    m_wizard.setContainer( this );
    wizard.addPages();

    updateControl();

    fireWizardChanged( wizard, IWizardContainerListener.REASON_NONE );

    showStartingPage();
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    disposeWizard();

    m_listeners.clear();
  }

  /**
   * Disposed off the current wizard
   */
  private void disposeWizard()
  {
    if( m_wizard != null )
    {
      //  todo: maybe ask for unsaved data?
      m_wizard.setContainer( null );
      m_wizard.dispose();
      m_wizard = null;
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    m_panel = new Composite( parent, SWT.NONE );
    m_panel.setLayout( new GridLayout() );
    m_panel.setFont( parent.getFont() );

    initializeDialogUnits( m_panel );

    updateControl();
  }

  private void updateControl()
  {
    if( m_panel == null || m_panel.isDisposed() )
      return;

    final Control[] children = m_panel.getChildren();
    for( int i = 0; i < children.length; i++ )
      children[i].dispose();

    if( m_wizard == null )
    {
      final Label label = new Label( m_panel, SWT.NONE );
      label.setText( "<wizard not set>" );
      label.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    }
    else
    {
      m_pageContainer = new Composite( m_panel, SWT.NONE );
      m_pageContainer.setLayout( m_stackLayout );

      m_pageContainer.setLayoutData( new GridData( GridData.FILL_BOTH ) );
      m_pageContainer.setFont( m_panel.getFont() );
      //    // Allow the wizard pages to precreate their page controls
      //    createPageControls();

      final Label label = new Label( m_panel, SWT.HORIZONTAL | SWT.SEPARATOR );
      label.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

      createButtonBar( m_panel );
    }

    m_panel.layout();
  }

  /**
   * Creates and returns the contents of this dialog's button bar.
   * <p>
   * The <code>Dialog</code> implementation of this framework method lays out a button bar and calls the
   * <code>createButtonsForButtonBar</code> framework method to populate it. Subclasses may override.
   * </p>
   * <p>
   * The returned control's layout data must be an instance of <code>GridData</code>.
   * </p>
   * 
   * @param parent
   *          the parent composite to contain the button bar
   * @return the button bar control
   */
  protected Control createButtonBar( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );

    // create a layout with spacing and margins appropriate for the font
    // size.
    final GridLayout layout = new GridLayout();
    layout.numColumns = 0; // this is incremented by createButton
    layout.makeColumnsEqualWidth = false;
    layout.marginWidth = convertHorizontalDLUsToPixels( IDialogConstants.HORIZONTAL_MARGIN );
    layout.marginHeight = convertVerticalDLUsToPixels( IDialogConstants.VERTICAL_MARGIN );
    layout.horizontalSpacing = convertHorizontalDLUsToPixels( IDialogConstants.HORIZONTAL_SPACING );
    layout.verticalSpacing = convertVerticalDLUsToPixels( IDialogConstants.VERTICAL_SPACING );
    composite.setLayout( layout );

    final GridData data = new GridData( GridData.HORIZONTAL_ALIGN_END | GridData.VERTICAL_ALIGN_CENTER );
    composite.setLayoutData( data );
    composite.setFont( parent.getFont() );

    // Add the buttons to the button bar.
    createButtonsForButtonBar( composite );

    return composite;
  }

  protected void createButtonsForButtonBar( final Composite parent )
  {
    if( m_wizard == null )
      return;

    if( m_wizard.isHelpAvailable() )
      createButton( parent, IDialogConstants.HELP_ID, IDialogConstants.HELP_LABEL, "doHelp", false );

    if( m_wizard.needsPreviousAndNextButtons() )
      createPreviousAndNextButtons( parent );

    createButton( parent, IDialogConstants.FINISH_ID, IDialogConstants.FINISH_LABEL, "doFinish", true );
    createButton( parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, "doCancel", false );
  }

  /**
   * Creates the Previous and Next buttons for this wizard dialog. Creates standard (<code>SWT.PUSH</code>) buttons
   * and registers for their selection events. Note that the number of columns in the button bar composite is
   * incremented. These buttons are created specially to prevent any space between them.
   * 
   * @param parent
   *          the parent button bar
   * @return a composite containing the new buttons
   */
  private Composite createPreviousAndNextButtons( Composite parent )
  {
    // increment the number of columns in the button bar
    ( (GridLayout)parent.getLayout() ).numColumns++;
    Composite composite = new Composite( parent, SWT.NONE );
    // create a layout with spacing and margins appropriate for the font size.
    GridLayout layout = new GridLayout();
    layout.numColumns = 0; // will be incremented by createButton
    layout.marginWidth = 0;
    layout.marginHeight = 0;
    layout.horizontalSpacing = 0;
    layout.verticalSpacing = 0;
    composite.setLayout( layout );
    GridData data = new GridData( GridData.HORIZONTAL_ALIGN_CENTER | GridData.VERTICAL_ALIGN_CENTER );
    composite.setLayoutData( data );
    composite.setFont( parent.getFont() );
    createButton( composite, IDialogConstants.BACK_ID, IDialogConstants.BACK_LABEL, "doPrev", false );
    createButton( composite, IDialogConstants.NEXT_ID, IDialogConstants.NEXT_LABEL, "doNext", false );
    return composite;
  }

  /**
   * Creates a new button with the given id.
   * <p>
   * The <code>Dialog</code> implementation of this framework method creates a standard push button, registers it for
   * selection events including button presses, and registers default buttons with its shell. The button id is stored as
   * the button's client data. If the button id is <code>IDialogConstants.CANCEL_ID</code>, the new button will be
   * accessible from <code>getCancelButton()</code>. If the button id is <code>IDialogConstants.OK_ID</code>, the
   * new button will be accesible from <code>getOKButton()</code>. Note that the parent's layout is assumed to be a
   * <code>GridLayout</code> and the number of columns in this layout is incremented. Subclasses may override.
   * </p>
   * 
   * @param parent
   *          the parent composite
   * @param id
   *          the id of the button (see <code>IDialogConstants.*_ID</code> constants for standard dialog button ids)
   * @param label
   *          the label from the button
   * @param defaultButton
   *          <code>true</code> if the button is to be the default button, and <code>false</code> otherwise
   * @param handlerMethod
   *          (java) name of the method which handles this button. Must be of kind 'public void xxx()'
   * 
   * @return the new button
   */
  protected Button createButton( final Composite parent, final int id, final String label, final String handlerMethod,
      final boolean defaultButton )
  {
    // increment the number of columns in the button bar
    ( (GridLayout)parent.getLayout() ).numColumns++;
    final Button button = new Button( parent, SWT.PUSH );
    button.setText( label );
    button.setFont( JFaceResources.getDialogFont() );

    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( final SelectionEvent event )
      {
        buttonPressed( handlerMethod );
      }
    } );
    if( defaultButton )
    {
      final Shell shell = parent.getShell();
      if( shell != null )
        shell.setDefaultButton( button );
    }

    m_buttons.put( new Integer( id ), button );
    setButtonLayoutData( button );
    return button;
  }

  protected void buttonPressed( final String handlerMethod )
  {
    try
    {
      final Method method = getClass().getMethod( handlerMethod, null );
      method.invoke( this, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Set the layout data of the button to a GridData with appropriate heights and widths.
   * 
   * @param button
   */
  protected void setButtonLayoutData( Button button )
  {
    final GridData data = new GridData( GridData.HORIZONTAL_ALIGN_FILL );
    data.heightHint = convertVerticalDLUsToPixels( IDialogConstants.BUTTON_HEIGHT );
    int widthHint = convertHorizontalDLUsToPixels( IDialogConstants.BUTTON_WIDTH );
    data.widthHint = Math.max( widthHint, button.computeSize( SWT.DEFAULT, SWT.DEFAULT, true ).x );
    button.setLayoutData( data );
  }

  /**
   * Initializes the computation of horizontal and vertical dialog units based on the size of current font.
   * <p>
   * This method must be called before any of the dialog unit based conversion methods are called.
   * </p>
   * 
   * @param control
   *          a control from which to obtain the current font
   */
  protected void initializeDialogUnits( Control control )
  {
    // Compute and store a font metric
    GC gc = new GC( control );
    gc.setFont( JFaceResources.getDialogFont() );
    m_fontMetrics = gc.getFontMetrics();
    gc.dispose();
  }

  /**
   * Returns the number of pixels corresponding to the given number of vertical dialog units.
   * <p>
   * This method may only be called after <code>initializeDialogUnits</code> has been called.
   * </p>
   * <p>
   * Clients may call this framework method, but should not override it.
   * </p>
   * 
   * @param dlus
   *          the number of vertical dialog units
   * @return the number of pixels
   */
  protected int convertVerticalDLUsToPixels( int dlus )
  {
    // test for failure to initialize for backward compatibility
    if( m_fontMetrics == null )
      return 0;
    return Dialog.convertVerticalDLUsToPixels( m_fontMetrics, dlus );
  }

  /**
   * Returns the number of pixels corresponding to the given number of horizontal dialog units.
   * <p>
   * This method may only be called after <code>initializeDialogUnits</code> has been called.
   * </p>
   * <p>
   * Clients may call this framework method, but should not override it.
   * </p>
   * 
   * @param dlus
   *          the number of horizontal dialog units
   * @return the number of pixels
   */
  protected int convertHorizontalDLUsToPixels( int dlus )
  {
    // test for failure to initialize for backward compatibility
    if( m_fontMetrics == null )
      return 0;
    return Dialog.convertHorizontalDLUsToPixels( m_fontMetrics, dlus );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    m_panel.setFocus();
  }

  /**
   * Not imlemented, as we are inside a ViewPart, which should'nt change its own size.
   * 
   * @see org.eclipse.jface.wizard.IWizardContainer2#updateSize()
   */
  public void updateSize()
  {}

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#getCurrentPage()
   */
  public IWizardPage getCurrentPage()
  {
    return m_currentPage;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#getShell()
   */
  public Shell getShell()
  {
    return getViewSite().getShell();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#showPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public void showPage( final IWizardPage page )
  {
    if( page == null || page == m_currentPage )
      return;

    // TODO: use this, when we implement back-pressed
    //    if( !isMovingToPreviousPage )
    // remember my previous page.
    page.setPreviousPage( m_currentPage );
    //    else
    //      isMovingToPreviousPage = false;
    //Update for the new page ina busy cursor if possible

    BusyIndicator.showWhile( m_panel.getDisplay(), new Runnable()
    {
      public void run()
      {
        updateForPage( page );
      }
    } );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateButtons()
   */
  public void updateButtons()
  {
    if( m_wizard == null || m_currentPage == null )
      return;

    boolean canFlipToNextPage = false;
    boolean canFinish = m_wizard.canFinish();

    final Button backButton = getButton( IDialogConstants.BACK_ID );
    final Button nextButton = getButton( IDialogConstants.NEXT_ID );
    final Button finishButton = getButton( IDialogConstants.FINISH_ID );
    if( backButton != null )
      backButton.setEnabled( m_currentPage.getPreviousPage() != null );

    if( nextButton != null )
    {
      canFlipToNextPage = m_currentPage.canFlipToNextPage();
      nextButton.setEnabled( canFlipToNextPage );
    }
    finishButton.setEnabled( canFinish );

    // finish is default unless it is diabled and next is enabled
    if( canFlipToNextPage && !canFinish )
      getShell().setDefaultButton( nextButton );
    else
      getShell().setDefaultButton( finishButton );
  }

  /**
   * Returns the button created by the method <code>createButton</code> for the specified ID as defined on
   * <code>IDialogConstants</code>. If <code>createButton</code> was never called with this ID, or if
   * <code>createButton</code> is overridden, this method will return <code>null</code>.
   * 
   * @param id
   *          the id of the button to look for
   * 
   * @return the button for the ID or <code>null</code>
   * 
   * @see #createButton(Composite, int, String, String, boolean)
   * @since 2.0
   */
  protected Button getButton( int id )
  {
    return (Button)m_buttons.get( new Integer( id ) );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateMessage()
   */
  public void updateMessage()
  {
  // we have no message area, do nothing
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateTitleBar()
   */
  public void updateTitleBar()
  {
  // we have no title bar, do nothing
  }

  /**
   * @see org.eclipse.jface.wizard.IWizardContainer#updateWindowTitle()
   */
  public void updateWindowTitle()
  {
    final String title = m_wizard.getWindowTitle();
    setPartName( title );
  }

  /**
   * @see org.eclipse.jface.operation.IRunnableContext#run(boolean, boolean,
   *      org.eclipse.jface.operation.IRunnableWithProgress)
   */
  public void run( final boolean fork, final boolean cancelable, final IRunnableWithProgress runnable )
      throws InvocationTargetException, InterruptedException
  {
    final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
    progressService.run( fork, cancelable, runnable );
  }

  /**
   * Shows the starting page of the wizard.
   */
  private void showStartingPage()
  {
    final IWizardPage startingPage = m_wizard.getStartingPage();
    if( startingPage == null )
    {
      // something must have happend getting the page
      return;
    }

    updateForPage( startingPage );
  }

  /**
   * Updates this dialog's controls to reflect the current page.
   */
  protected void update()
  {
    // Update the window title
    updateWindowTitle();
    // Update the title bar
    updateTitleBar();
    // Update the buttons
    updateButtons();
  }

  /**
   * Update the receiver for the new page.
   * 
   * @param page
   */
  protected void updateForPage( final IWizardPage page )
  {
    // ensure this page belongs to the current wizard
    if( m_wizard != page.getWizard() )
      throw new IllegalArgumentException();

    // ensure that page control has been created
    // (this allows lazy page control creation)
    if( page.getControl() == null )
    {
      page.createControl( m_pageContainer );
      // the page is responsible for ensuring the created control is accessable
      // via getControl.
      final Control control = page.getControl();
      Assert.isNotNull( control );

      // ensure the dialog is large enough for this page
      //      updateSize( page );
    }

    // make the new page visible
    //    final IWizardPage oldPage = m_currentPage;
    m_currentPage = page;

    m_stackLayout.topControl = m_currentPage.getControl();
    //    m_currentPage.setVisible( true );
    //    if( oldPage != null )
    //      oldPage.setVisible( false );

    // update the dialog controls
    m_pageContainer.layout();
    update();

    firePageChanged( page );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.view.IWizardContainer3#addWizardContainerListener(org.kalypso.contribs.eclipse.jface.wizard.view.IWizardContainerListener)
   */
  public void addWizardContainerListener( final IWizardContainerListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.view.IWizardContainer3#removeWizardContainerListener(org.kalypso.contribs.eclipse.jface.wizard.view.IWizardContainerListener)
   */
  public void removeWizardContainerListener( final IWizardContainerListener l )
  {
    m_listeners.remove( l );
  }

  protected final void fireWizardChanged( final IWizard newwizard, final int reason )
  {
    final IWizardContainerListener[] listeners = (IWizardContainerListener[])m_listeners
        .toArray( new IWizardContainerListener[m_listeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
    {
      final IWizardContainerListener listener = listeners[i];
      Platform.run( new SafeRunnable()
      {
        public void run() throws Exception
        {
          listener.onWizardChanged( newwizard, reason );
        }
      } );
    }
  }

  protected final void firePageChanged( final IWizardPage newpage )
  {
    final IWizardContainerListener[] listeners = (IWizardContainerListener[])m_listeners
        .toArray( new IWizardContainerListener[m_listeners.size()] );
    for( int i = 0; i < listeners.length; i++ )
    {
      final IWizardContainerListener listener = listeners[i];
      Platform.run( new SafeRunnable()
      {
        public void run() throws Exception
        {
          listener.onPageChanged( newpage );
        }
      } );
    }
  }

  public IWizard getWizard()
  {
    return m_wizard;
  }

  public void doNext()
  {
    final IWizardPage currentPage = getCurrentPage();
    final IWizard wizard = getWizard();

    if( wizard == null || currentPage == null )
      return;

    if( wizard instanceof IWizard2 )
    {
      if( !( (IWizard2)wizard ).finishPage( currentPage ) )
        return;
    }

    final IWizardPage nextPage = currentPage.getNextPage();
    if( nextPage == null )
      return;

    showPage( nextPage );
  }

  public void doPrev()
  {
    final IWizardPage currentPage = getCurrentPage();
    if( currentPage == null )
      return;

    final IWizardPage previousPage = currentPage.getPreviousPage();
    if( previousPage == null )
      return;

    showPage( previousPage );
  }

  public void doFinish()
  {
    final IWizard wizard = getWizard();

    if( wizard == null )
      return;

    if( wizard.performFinish() )
    {
      disposeWizard();
      fireWizardChanged( null, IWizardContainerListener.REASON_FINISHED );
    }
  }

  public void doCancel()
  {
    final IWizard wizard = getWizard();

    if( wizard == null )
      return;

    if( wizard.performCancel() )
    {
      disposeWizard();
      fireWizardChanged( null, IWizardContainerListener.REASON_CANCELED );
    }
  }
}
