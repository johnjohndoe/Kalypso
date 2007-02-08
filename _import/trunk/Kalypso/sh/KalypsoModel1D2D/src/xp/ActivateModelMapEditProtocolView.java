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
package xp;

import java.util.logging.Logger;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;

/*
 * TODO have a look aber IWidgetWithOption and it showing view why not use that
 */
/**
 * Activates the model map edit protocol view
 * 
 * @author Patrice Congo
 */
public class ActivateModelMapEditProtocolView extends AbstractWidget implements IWidgetWithOptions
{
  public static final String NAME = "activate_1d_2d_draw_panel";

  public static final String TOOL_TIP = "1d 2d zeichen toobal activieren";

  private static final Logger logger = Logger.getLogger( ActivateModelMapEditProtocolView.class.getName() );

  private ModelMapEditStrategyView protoView;

  public ActivateModelMapEditProtocolView( )
  {
    super( NAME, TOOL_TIP );
  }

  public ActivateModelMapEditProtocolView( String name, String toolTip )
  {
    super( name, toolTip );
  }

  private void test( )
  {
    // this.getMapPanel();
    // this.activate( commandPoster, mapPanel )
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    logger.info( "activate" );
    // //PlatformUI.getWorkbench().getService( Object.class );
    // IWorkbench workbench= PlatformUI.getWorkbench();
    // // ModelMapEditProtocolView protoView=
    // // (ModelMapEditProtocolView)
    // // workbench.getViewRegistry().find(ModelMapEditProtocolView.ID);
    // IWorkbenchPage page=
    // workbench.getActiveWorkbenchWindow().getActivePage();
    // try
    // {
    // //TODO does not work because of AbstractWidget.activate() which register again
    // // page.showView(ModelMapEditProtocolView.ID);
    // protoView=
    // (ModelMapEditStrategyView)page.showView(
    // ModelMapEditStrategyView.ID,
    // null,//ModelMapEditProtocolView.ID,
    // IWorkbenchPage.VIEW_VISIBLE );
    // }
    // catch( PartInitException e )
    // {
    // logger.error( "Could not open protocol view", e);
    // }

    super.activate( commandPoster, mapPanel );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    IWorkbench workbench = PlatformUI.getWorkbench();
    IWorkbenchPage page = workbench.getActiveWorkbenchWindow().getActivePage();

    page.hideView( protoView );
    logger.info( "finish" );
    super.finish();
  }

  private Label label;

  // private Text textField;
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( Composite parent )
  {
    label = new Label( parent, SWT.NONE );
    label.setText( "DADADADDA" );
    // textField=new Text(parent,SWT.NONE);
    // textField.setText( "TTTTTTTTTTTTTTTTTTTTTTTTTTT" );

    return label;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( label == null )
    {
      return;
    }
    if( !label.isDisposed() )
    {
      label.dispose();
    }
  }

  private static final void doDispose( Control control )
  {
    if( control == null )
    {
      return;
    }
    if( control.isDisposed() )
    {
      return;
    }
    control.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( ISelection selection )
  {
    // textField.setText( selection.toString() );
    System.out.println( selection );
    super.setSelection( selection );
  }
}
