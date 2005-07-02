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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.util.Assert;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
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

  /** Sets an new wizard and immediately displays its first page */
  public void setWizard( final IWizard wizard )
  {
    disposeWizard();

    m_wizard = wizard;
    m_wizard.setContainer( this );
    wizard.addPages();

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
    m_pageContainer = new Composite( parent, SWT.NONE );
    m_pageContainer.setLayout( m_stackLayout );

    m_pageContainer.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    m_pageContainer.setFont( parent.getFont() );

    //    // Allow the wizard pages to precreate their page controls
    //    createPageControls();
    //    // Show the first page
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    m_pageContainer.setFocus();
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

    BusyIndicator.showWhile( m_pageContainer.getDisplay(), new Runnable()
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
  // we have no buttons, do nothing
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
  public void run( final boolean fork, final boolean cancelable, final IRunnableWithProgress runnable ) throws InvocationTargetException,
      InterruptedException
  {
    final IProgressService progressService = PlatformUI.getWorkbench().getProgressService();
    progressService.run(fork, cancelable, runnable);
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
