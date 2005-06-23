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
package org.kalypso.ui.editor.gistableeditor;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.gistableeditor.actions.ColumnAction;
import org.kalypso.ui.preferences.IKalypsoPreferences;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * <p>
 * Eclipse-Editor zum editieren der Gis-Tabellen-Templates.
 * </p>
 * 
 * <p>
 * Zeigt das ganze als Tabelendarstellung, die einzelnen Datenquellen k?nnen potentiell editiert werden
 * </p>
 * 
 * @author belger
 */
public class GisTableEditor extends AbstractEditorPart implements ISelectionProvider
{
  private final ObjectFactory m_gistableviewFactory = new ObjectFactory();

  private final Marshaller m_marshaller;

  private LayerTableViewer m_layerTable = null;

  public GisTableEditor()
  {
    try
    {
      m_marshaller = m_gistableviewFactory.createMarshaller();
      m_marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    }
    catch( final JAXBException e )
    {
      // sollte nie passieren
      e.printStackTrace();

      throw new RuntimeException( e );
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    m_layerTable.dispose();

    super.dispose();
  }

  /**
   * File must exist!
   * 
   * @param monitor
   * @param input
   */
  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input )
  {
    if( m_layerTable == null )
      return;

    try
    {
      final Gistableview tableTemplate = m_layerTable.createTableTemplate();

      // die Vorlagendatei ist klein, deswegen einfach in ein ByteArray
      // serialisieren
      final IFile file = input.getFile();

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      final OutputStreamWriter osw = new OutputStreamWriter( bos, file.getCharset() );
      m_marshaller.marshal( tableTemplate, osw );

      // TODO close in finally block?
      bos.close();

      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
      file.setContents( bis, false, true, monitor );

      // TODO close in finally block?
      bis.close();
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    final IFeatureModifierFactory factory = plugin.createFeatureTypeCellEditorFactory();
    m_layerTable = new LayerTableViewer( parent, SWT.BORDER, this, factory, plugin.getDefaultMapSelectionID() );

    MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( new Separator() );
        //    mgr.add(selectAllAction);
        appendSpaltenActions( manager );
      }
    } );

    Menu menu = menuManager.createContextMenu( m_layerTable.getControl() );
    m_layerTable.getControl().setMenu( menu );

    getSite().registerContextMenu( menuManager, m_layerTable );
    load();
  }

  protected final void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception
  {
    if( !( input instanceof IFileEditorInput ) )
      throw new IllegalArgumentException( "Kann nur Dateien laden" );

    if( m_layerTable == null )
      return;

    monitor.beginTask( "Vorlage laden", 1000 );

    final Gistableview tableTemplate = GisTemplateHelper.loadGisTableview( ( (IFileEditorInput)input ).getFile() );

    final IFile inputFile = ( (IFileEditorInput)getEditorInput() ).getFile();
    final URL context = ResourceUtilities.createURL( inputFile );

    final LayerTableViewer viewer = m_layerTable;
    getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      public void run()
      {
        viewer.applyTableTemplate( tableTemplate, context );
      }
    } );

    monitor.worked( 1000 );
  }

  public LayerTableViewer getLayerTable()
  {
    return m_layerTable;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#addSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void addSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_layerTable.addSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#getSelection()
   */
  public ISelection getSelection()
  {
    return m_layerTable.getSelection();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#removeSelectionChangedListener(org.eclipse.jface.viewers.ISelectionChangedListener)
   */
  public void removeSelectionChangedListener( ISelectionChangedListener listener )
  {
    m_layerTable.removeSelectionChangedListener( listener );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionProvider#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    m_layerTable.setSelection( selection );
  }

  public void appendSpaltenActions( final IMenuManager manager )
  {
    final IKalypsoFeatureTheme theme = m_layerTable.getTheme();
    if( theme == null )
      return;

    final FeatureTypeProperty[] ftps = theme.getFeatureType().getProperties();
    //    final String lang = Locale.getDefault().getLanguage();
    final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString( IKalypsoPreferences.LANGUAGE );

    for( int i = 0; i < ftps.length; i++ )
    {
      manager.add( new ColumnAction( this, m_layerTable, ftps[i].getName(), ftps[i].getAnnotation( lang ) ) );
    }
  }

  //  public void createSpaltenMenu( IMenuManager menuManager )
  //  {
  //    // menuManager.setRemoveAllWhenShown( true );
  //    menuManager.addMenuListener( new IMenuListener()
  //    {
  //      public void menuAboutToShow( final IMenuManager manager )
  //      {
  //        manager.removeAll();
  //        appendSpaltenActions( manager );
  //      }
  //    } );
  //    
  //    
  //    // final MenuManager menuMgr = new MenuManager( "Spalten", id );
  //    // menuMgr.setRemoveAllWhenShown( true );
  //    // menuMgr.addMenuListener( new IMenuListener()
  //    // {
  //    // public void menuAboutToShow( final IMenuManager manager )
  //    // {
  //    // appendSpaltenActions( manager );
  //    // }
  //    // } );
  //    //
  //    // return menuMgr;
  //  }
}