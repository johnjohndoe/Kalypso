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
import java.io.OutputStreamWriter;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.IOUtils;
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
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.ogc.gml.table.wizard.ExportTableOptionsPage;
import org.kalypso.ogc.gml.table.wizard.ExportableLayerTable;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.gistableview.ObjectFactory;
import org.kalypso.ui.ImageProvider;
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
public class GisTableEditor extends AbstractEditorPart implements ISelectionProvider, IExportableObjectFactory
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
    getSite().setSelectionProvider( this );
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

    ByteArrayOutputStream bos = null;
    ByteArrayInputStream bis = null;
    try
    {
      final Gistableview tableTemplate = m_layerTable.createTableTemplate();

      // die Vorlagendatei ist klein, deswegen einfach in ein ByteArray
      // serialisieren
      final IFile file = input.getFile();

      bos = new ByteArrayOutputStream();
      final OutputStreamWriter osw = new OutputStreamWriter( bos, file.getCharset() );
      m_marshaller.marshal( tableTemplate, osw );
      bos.close();

      bis = new ByteArrayInputStream( bos.toByteArray() );
      file.setContents( bis, false, true, monitor );
      bis.close();

    }
    catch( final Exception e )
    {
      e.printStackTrace();

      // TODO error handling
    }
    finally
    {
      IOUtils.closeQuietly( bos );
      IOUtils.closeQuietly( bis );
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
    m_layerTable = new LayerTableViewer( parent, SWT.BORDER, this, factory, KalypsoCorePlugin.getDefault().getSelectionManager() );

    final MenuManager menuManager = new MenuManager();
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

    getSite().registerContextMenu( menuManager, m_layerTable );
    getSite().setSelectionProvider( getLayerTable() );
    m_layerTable.setMenu( menuManager );

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
    // TODO: use platform mechanism instead:
    //    Platform.getNL();
    final String lang = KalypsoGisPlugin.getDefault().getPluginPreferences().getString( IKalypsoPreferences.LANGUAGE );

    for( int i = 0; i < ftps.length; i++ )
      manager.add( new ColumnAction( this, m_layerTable, ftps[i].getName(), ftps[i].getAnnotation( lang ) ) );
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IExportableObjectFactory.class )
      return this;
    
    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createExportableObjects(org.apache.commons.configuration.Configuration)
   */
  public IExportableObject[] createExportableObjects( Configuration configuration ) throws CoreException
  {
    final ExportableLayerTable exp = new ExportableLayerTable( m_layerTable );

    return new IExportableObject[]
    { exp };
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createWizardPages(org.kalypso.metadoc.configuration.IPublishingConfiguration)
   */
  public IWizardPage[] createWizardPages( IPublishingConfiguration configuration ) throws CoreException
  {
    final IWizardPage page = new ExportTableOptionsPage( "optionPage", "Export Otionen",
        ImageProvider.IMAGE_UTIL_BERICHT_WIZ );

    return new IWizardPage[]
    { page };
  }
}