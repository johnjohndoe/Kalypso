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
package org.kalypso.ui.wizards.results;

import java.math.BigDecimal;
import java.util.Arrays;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectImages.DESCRIPTORS;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypso.ui.wizards.results.editor.EditStyleDialog;
import org.kalypso.ui.wizards.results.editor.IEditStyleDialogModifyListener;

/**
 * @author Thomas Jung
 * 
 */
public class ResultStyleComposite
{

  private ComboViewer m_combStyleTemplates;

  // private IFile m_style;

  private final String m_stylePath;

  private final BigDecimal m_minValue;

  private final BigDecimal m_maxValue;

  private final IFolder m_scenarioFolder;

  private Button m_editStyleButton;

  private Button m_removeStyleButton;

  private final ResultAddLayerCommandData m_resultAddLayerCommandData;

  public ResultStyleComposite( final Composite parent, final IFolder scenarioFolder, final String stylePath, final BigDecimal minValue, final BigDecimal maxValue, final ResultAddLayerCommandData resultAddLayerCommandData )
  {
    m_scenarioFolder = scenarioFolder;
    m_stylePath = stylePath;

    /* min / max values of the document */
    m_minValue = minValue;
    m_maxValue = maxValue;
    m_resultAddLayerCommandData = resultAddLayerCommandData;

    createComboControl( parent );

    createEditButtonControl( parent );

    createRemoveButtonControl( parent );
    
//    m_resultAddLayerCommandData.getDocumentResult().ge
  }

  private void createRemoveButtonControl( final Composite parent )
  {
    m_removeStyleButton = new Button( parent, SWT.FLAT );
    final String tooltipRemove = Messages.getString("org.kalypso.ui.wizards.results.ResultStyleComposite.0"); //$NON-NLS-1$
    m_removeStyleButton.setToolTipText( tooltipRemove );
    final Image removeImage = Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_VIEWER_REMOVE ).createImage();
    m_removeStyleButton.setImage( removeImage );

    m_removeStyleButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        try
        {
          // get selection
          Object element = m_combStyleTemplates.getElementAt( m_combStyleTemplates.getCombo().getSelectionIndex() );
          IFile fileElement = (IFile) element;
          // delete file
          fileElement.delete( true, true, new NullProgressMonitor() );
          refreshCombo();
        }
        catch( CoreException e1 )
        {
          e1.printStackTrace();
        }
      }
    } );
  }

  private void createEditButtonControl( final Composite parent )
  {
    m_editStyleButton = new Button( parent, SWT.FLAT );
    final String tooltipEdit = Messages.getString("org.kalypso.ui.wizards.results.ResultStyleComposite.1"); //$NON-NLS-1$
    m_editStyleButton.setToolTipText( tooltipEdit );

    final Image editImage = Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_VIEWER_EDIT ).createImage();
    m_editStyleButton.setImage( editImage );

    m_editStyleButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_resultAddLayerCommandData.getSldFile() != null )
        {
          EditStyleDialog styleEditor = new EditStyleDialog( m_editStyleButton.getShell(), m_resultAddLayerCommandData, m_minValue, m_maxValue );
          styleEditor.addModifyListener( new IEditStyleDialogModifyListener()
          {
            @Override
            public void onStyleChanged( Object source, IFile sldFile )
            {
              m_resultAddLayerCommandData.setSldFile( sldFile );
              refreshCombo();
            }
          } );
          styleEditor.open();
        }
      }
    } );
  }

  private void createComboControl( final Composite parent )
  {
    m_combStyleTemplates = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_combStyleTemplates.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_combStyleTemplates.getControl().setToolTipText( Messages.getString("org.kalypso.ui.wizards.results.ResultStyleComposite.2") ); //$NON-NLS-1$
    m_combStyleTemplates.setContentProvider( new ArrayContentProvider() );

    refreshCombo();

    m_combStyleTemplates.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( Object element )
      {
        if( element instanceof IFile )
        {
          IFile fileElement = (IFile) element;
          return FileUtilities.nameWithoutExtension( fileElement.getName() );
        }
        else
          return super.getText( element );
      }
    } );

    // selection listener
    m_combStyleTemplates.addSelectionChangedListener( new ISelectionChangedListener()
    {

      @Override
      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object element = selection.getFirstElement();
        if( element instanceof IFile )
        {
          m_resultAddLayerCommandData.setSldFile( (IFile) element );
        }
      }
    } );
  }

  protected void refreshCombo( )
  {
    // looking for existing scenario style templates and fill them into the combo
    final IFile[] styleTemplates = getStyleTemplates( m_stylePath, m_scenarioFolder );
    m_combStyleTemplates.setInput( styleTemplates );

    final Object elementAt = m_combStyleTemplates.getElementAt( 0 );

    if( m_resultAddLayerCommandData.getSldFile() == null )
    {
      if( elementAt != null )
      {
        m_combStyleTemplates.setSelection( new StructuredSelection( elementAt ) );

        IStructuredSelection selection = (IStructuredSelection) m_combStyleTemplates.getSelection();
        Object firstElement = selection.getFirstElement();

        if( firstElement instanceof IFile )
          m_resultAddLayerCommandData.setSldFile( (IFile) firstElement );
      }
    }
    else
    {
      if( Arrays.asList( styleTemplates ).contains( m_resultAddLayerCommandData.getSldFile() ) )
      {
        m_combStyleTemplates.setSelection( new StructuredSelection( m_resultAddLayerCommandData.getSldFile() ) );
      }
      else if( elementAt != null )
      {
        m_combStyleTemplates.setSelection( new StructuredSelection( elementAt ) );
      }
    }
  }

  private IFile[] getStyleTemplates( final String path, IFolder scenarioFolder )
  {
    final IFolder stylesFolder = KalypsoModel1D2DHelper.getStylesFolder( scenarioFolder );
    IFolder folder = stylesFolder.getFolder( path );

    return ResourceUtilities.getChildrenWithExtensionQuiet( folder, IResource.DEPTH_ONE, "sld" ); //$NON-NLS-1$
  }

  public void setEnabled( boolean status )
  {
    m_combStyleTemplates.getControl().setEnabled( status );
    m_editStyleButton.setEnabled( status );
    m_removeStyleButton.setEnabled( status );
  }

}
