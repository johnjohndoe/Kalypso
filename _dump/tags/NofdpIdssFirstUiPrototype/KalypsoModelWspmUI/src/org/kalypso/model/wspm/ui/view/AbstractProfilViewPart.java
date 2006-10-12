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
package org.kalypso.model.wspm.ui.view;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.editor.IProfilchartEditorListener;
import org.kalypso.model.wspm.ui.editor.ProfilchartEditor;
import org.kalypso.model.wspm.ui.profil.operation.ProfilUndoContext;
import org.kalypso.model.wspm.ui.profil.view.IProfilViewDataListener;
import org.kalypso.model.wspm.ui.profil.view.ProfilViewData;
import org.kalypso.model.wspm.ui.profil.view.chart.action.StatusPosContributionItem;


/**
 * @author belger
 */
public abstract class AbstractProfilViewPart extends ViewPart implements IProfilViewPart,
    IProfilchartEditorListener, IProfilViewDataListener
{
  private ProfilchartEditor m_editor;

  private Composite m_control;

  private ActionGroup m_actionGroup;

  private StatusPosContributionItem m_statusLineItem = new StatusPosContributionItem( "pos" );

  @Override
  public void init( final IViewSite site ) throws PartInitException
  {
    super.init( site );

    final IActionBars actionBars = site.getActionBars();
    actionBars.getStatusLineManager().add( m_statusLineItem );

    // we want to register the globalActions (from the profileditor) here
    // but we may have not editor yes
    // so we register instead everytime setActiveEditor is called
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#dispose()
   */
  @Override
  public void dispose( )
  {
    super.dispose();

    if( m_actionGroup != null )
    {
      m_actionGroup.dispose();
      m_actionGroup = null;
    }

    if( m_editor != null )
    {
      saveState();
      m_editor.removeProfilchartEditorListener( this );
      m_editor.getViewData().removeProfilViewDataListener( this );
    }
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public final void createPartControl( final Composite parent )
  {
    m_control = new Composite( parent, SWT.NONE );
    m_control.setLayout( new GridLayout() );

    final IEditorPart activeEditor = getSite().getPage().getActiveEditor();
    if( activeEditor instanceof ProfilchartEditor )
      setProfilchartEditor( (ProfilchartEditor)activeEditor );
  }

  public final ProfilViewData getViewData( )
  {
    return m_editor == null ? null : m_editor.getViewData();
  }

  public final ProfilchartEditor getProfilchartEditor( )
  {
    return m_editor;
  }

  public final void setProfilchartEditor( final ProfilchartEditor editor )
  {
    if( m_editor == editor )
      return;

    setContentDescription( "" );
    setPartName( "" );

    if( m_editor != null )
    {
      saveState();
      m_editor.removeProfilchartEditorListener( this );
      m_editor.getViewData().removeProfilViewDataListener( this );
    }

    m_editor = editor;

    if( m_editor != null )
    {
      m_editor.addProfilchartEditorListener( this );

      final ProfilViewData viewData = m_editor.getViewData();
      viewData.addProfilViewDataListener( this );

      setPartName( m_editor.getPartName() );
      setContentDescription( m_editor.getContentDescription() );
    }

//    m_statusLineItem.setEditor( editor );

    if( m_editor != null )
    {
      final IActionBars bars = getViewSite().getActionBars();
      m_editor.registerCommonGlobalActions( bars );
      bars.updateActionBars();
    }

    onProfilChanged( m_editor, m_editor == null ? null : m_editor.getProfil() );
  }

  @Override
  public void setFocus( )
  {
    m_control.setFocus();
  }

  /**
   * @see com.bce.profil.eclipse.editor.IProfilchartEditorListener#onProfilChanged(com.bce.profil.eclipse.editor.ProfilchartEditor,
   *      com.bce.profil.profilinterface.IProfil)
   */
  public final void onProfilChanged( final ProfilchartEditor editor, final IProfil newprofil )
  {
    if( m_actionGroup != null )
    {
      m_actionGroup.dispose();
      m_actionGroup = null;
    }

    if( editor != null )
    {
      m_actionGroup = new UndoRedoActionGroup( editor.getSite(), new ProfilUndoContext( editor
          .getProfil() ), true );
      final IActionBars actionBars = getViewSite().getActionBars();
      m_actionGroup.fillActionBars( actionBars );
    }

    if( m_control != null && !m_control.isDisposed() )
      m_control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          final Composite parent = getControl();
          if( parent == null || parent.isDisposed() )
            return;

          for( final Control c : parent.getChildren() )
            c.dispose();

          final Control control = createContent( parent );
          if( control != null )
            control.setLayoutData( new GridData( GridData.FILL_BOTH ) );
          parent.layout();
        }
      } );
  }

  protected Composite getControl( )
  {
    return m_control;
  }

  protected abstract Control createContent( final Composite parent );

  protected abstract void saveState( );
}
