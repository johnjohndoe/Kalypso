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
package org.kalypso.metadoc.ui;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.wizard.IWizardNode;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardSelectionPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.contribs.java.lang.DisposeHelper;
import org.kalypso.metadoc.IExportTarget;
import org.kalypso.metadoc.IExporter;
import org.kalypso.metadoc.KalypsoMetaDocPlugin;

/**
 * @author belger
 */
public class ExportDocumentsWizard extends Wizard
{
  private final IExporter[] m_exporter;
  private final Shell m_shell;
  private final DisposeHelper m_disposer;
  private final IExportTarget m_target;

  public ExportDocumentsWizard( final Shell shell, final IExporter[] exporter, final IExportTarget target )
  {
    m_shell = shell;
    m_exporter = exporter;
    m_target = target;
    m_disposer = new DisposeHelper();

    setForcePreviousAndNextButtons( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose()
  {
    m_disposer.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  @Override
  public void addPages()
  {
    final ImageDescriptor imageDescriptor = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoMetaDocPlugin.getId(), "icons/metadoc_wiz.gif" );

    // wizard selection for each exporter
    final IWizardNode[] nodes = new IWizardNode[m_exporter.length];
    for( int i = 0; i < nodes.length; i++ )
    {
      final IExporter exporter = m_exporter[i];
      nodes[i] = new ExportWizardNode( m_target, exporter, m_shell, imageDescriptor );
      m_disposer.addDisposeCandidate( nodes[i] );
    }

    final WizardSelectionPage page = new WizardSelectionPage( "selectionPage" )
    {
      public void createControl( final Composite parent )
      {
        final TableViewer lv = new TableViewer( parent, SWT.BORDER | SWT.SINGLE );
        lv.setLabelProvider( new ExporterWizardNodeLabelProvider() );
        lv.setContentProvider( new ArrayContentProvider() );
        lv.setInput( nodes );
        lv.addSelectionChangedListener( new ISelectionChangedListener()
        {
          public void selectionChanged( final SelectionChangedEvent event )
          {
            final IStructuredSelection selection = (IStructuredSelection)event.getSelection();
            setSelectedNode( (IWizardNode)selection.getFirstElement() );
          }
        } );
        if( nodes.length > 0 )
          lv.setSelection( new StructuredSelection( nodes[0] ) );

        setControl( lv.getControl() );
      }
      
      /**
       * @see org.eclipse.jface.wizard.WizardSelectionPage#setSelectedNode(org.eclipse.jface.wizard.IWizardNode)
       */
      @Override
      protected void setSelectedNode( final IWizardNode node )
      {
        super.setSelectedNode( node );
        
        if( node == null )
          setMessage( null );
        else
          setMessage( node.toString() );
      }
    };

    page.setTitle( "Wählen Sie die Exportart" );
    page.setImageDescriptor( imageDescriptor );
    
    addPage( page );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#performFinish()
   */
  @Override
  public boolean performFinish()
  {
    return true;
  }
  
  protected static final class ExporterWizardNodeLabelProvider extends LabelProvider
  {
    /** key -> image */
    private final Map<ExportWizardNode, Image> m_images = new HashMap<ExportWizardNode, Image>();

    /**
     * @see org.eclipse.jface.viewers.LabelProvider#dispose()
     */
    @Override
    public void dispose()
    {
      new DisposeHelper( m_images.values() ).dispose();

      super.dispose();
    }

    @Override
    public String getText( final Object element )
    {
      final ExportWizardNode node = (ExportWizardNode)element;
      return node.getExporter().getName();
    }

    @Override
    public Image getImage( final Object element )
    {
      final ExportWizardNode node = (ExportWizardNode)element;

      if( !m_images.containsKey( node ) )
      {
        final IExporter exporter = node.getExporter();
        final ImageDescriptor imageDescriptor = exporter.getImageDescriptor();
        m_images.put( node, imageDescriptor.createImage( true ) );
      }
      
      return m_images.get( node );
    }
  }
}
