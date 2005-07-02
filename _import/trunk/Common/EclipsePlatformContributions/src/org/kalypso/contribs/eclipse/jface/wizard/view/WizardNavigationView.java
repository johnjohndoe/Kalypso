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

import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.LocationAdapter;
import org.eclipse.swt.browser.LocationEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.contribs.eclipse.ui.PartAdapter2;

/**
 * The navigation are for a {@link org.kalypso.contribs.eclipse.jface.wizard.view.WizardView}.
 * 
 * @author belger
 */
public class WizardNavigationView extends ViewPart implements IWizardContainerListener
{
  private IWizard m_wizard;

  private static final String DEFAULT_HTML = "<html><body><form>"
      + "<button onClick=\"self.location.href='prev'\">Zurück</button>"
      + "<button onClick=\"self.location.href='next'\">Weiter</button>"
      + "<button onClick=\"self.location.href='finish'\">Beenden</button>"
      + "<button onClick=\"self.location.href='cancel'\">Abbrechen</button>"
      + "</form></body></html>";

  private Composite m_panel;

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    m_panel = new Composite( parent, SWT.NONE );
    m_panel.setLayout( new FillLayout() );

    updateControl();
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite)
   */
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IWorkbenchPage page = site.getPage();
    page.addPartListener( new PartAdapter2()
    {
      /**
       * @see org.kalypso.contribs.eclipse.ui.PartAdapter2#partOpened(org.eclipse.ui.IWorkbenchPartReference)
       */
      public void partOpened( final IWorkbenchPartReference partRef )
      {
        final IWorkbenchPart part = partRef.getPart( false );
        if( part instanceof WizardView )
        {
          final WizardView wizardView = (WizardView)part;
          wizardView.addWizardContainerListener( WizardNavigationView.this );
          onWizardChanged( wizardView.getWizard(), REASON_NONE );
        }
      }

      /**
       * @see org.kalypso.contribs.eclipse.ui.PartAdapter2#partClosed(org.eclipse.ui.IWorkbenchPartReference)
       */
      public void partClosed( final IWorkbenchPartReference partRef )
      {
        final IWorkbenchPart part = partRef.getPart( false );
        if( part == WizardNavigationView.this )
          page.removePartListener( this );
        else if( part instanceof WizardView )
        {
          final WizardView wizardView = (WizardView)part;
          wizardView.removeWizardContainerListener( WizardNavigationView.this );
          onWizardChanged( null, REASON_NONE );
        }
      }
    } );

    final WizardView part = (WizardView)page.findView( WizardView.class.getName() );
    if( part != null )
    {
      part.addWizardContainerListener( this );
      onWizardChanged( part.getWizard(), REASON_NONE );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#dispose()
   */
  public void dispose()
  {
    final WizardView part = (WizardView)getSite().getPage().findView( WizardView.class.getName() );
    if( part != null )
    {
      part.removeWizardContainerListener( this );
      onWizardChanged( null, REASON_NONE );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#setFocus()
   */
  public void setFocus()
  {
    m_panel.setFocus();
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.view.IWizardContainerListener#onWizardChanged(org.eclipse.jface.wizard.IWizard,
   *      int)
   */
  public void onWizardChanged( final IWizard newwizard, final int reason )
  {
    m_wizard = newwizard;
    updateControl();
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.wizard.view.IWizardContainerListener#onPageChanged(org.eclipse.jface.wizard.IWizardPage)
   */
  public void onPageChanged( final IWizardPage newpage )
  {
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
      label.setText( "<keine Wizard vorhanden >" );
    }
    else
    {
      final IWizardPage currentPage = m_wizard.getContainer().getCurrentPage();
      if( currentPage == null )
      {
        final Label label = new Label( m_panel, SWT.NONE );
        label.setText( "<aktuelle Seite nicht gesetzt>" );
      }
      else
      {
        final Browser browser = new Browser( m_panel, SWT.NONE );

        browser.setText( DEFAULT_HTML );

        browser.addLocationListener( new LocationAdapter()
        {
          /**
           * @see org.eclipse.swt.browser.LocationAdapter#changed(org.eclipse.swt.browser.LocationEvent)
           */
          public void changed( final LocationEvent event )
          {
            changeLocation( event.location );
          }
        } );
      }
    }

    m_panel.layout();
  }

  protected void changeLocation( final String location )
  {
    if( m_wizard == null )
      return;

    final String link;
    if( location.startsWith( "about:blank" ) )
      link = location.substring( "about:blank".length() );
    else
      link = location;

    if( link.length() == 0 )
      return;

    final WizardView wizardView = (WizardView)m_wizard.getContainer();

    updateControl();

    if( "prev".compareToIgnoreCase( link ) == 0 )
      wizardView.doPrev();
    else if( "next".compareToIgnoreCase( link ) == 0 )
      wizardView.doNext();
    else if( "finish".compareToIgnoreCase( link ) == 0 )
      wizardView.doFinish();
    else if( "cancel".compareToIgnoreCase( link ) == 0 )
      wizardView.doCancel();
    else
    {
      // jump to page with name 'location'
    }

    updateControl();
  }
}
