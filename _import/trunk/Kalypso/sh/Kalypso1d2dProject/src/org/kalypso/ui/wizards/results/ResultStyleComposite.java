/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
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
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectImages.DESCRIPTORS;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;

/**
 * @author Thomas Jung
 * 
 */
public class ResultStyleComposite
{

  private final ComboViewer m_combStyleTemplates;

  private final Button m_editStyleButton;

  private final Button m_removeLineStyleButton;

  private IFile m_style;

  public ResultStyleComposite( final Composite parent, IFolder scenarioFolder, String stylePath )
  {
    /* style combo */
    m_combStyleTemplates = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    m_combStyleTemplates.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    m_combStyleTemplates.getControl().setToolTipText( "Wählen Sie den geünschten Style aus" );
    m_combStyleTemplates.setContentProvider( new ArrayContentProvider() );

    // look for existing scenario style templates and fill them into the combo
    final IFile[] styleTemplates = getStyleTemplates( stylePath, scenarioFolder );
    m_combStyleTemplates.setInput( styleTemplates );
    if( m_combStyleTemplates.getElementAt( 0 ) != null )
    {
      m_combStyleTemplates.setSelection( new StructuredSelection( m_combStyleTemplates.getElementAt( 0 ) ) );

      IStructuredSelection selection = (IStructuredSelection) m_combStyleTemplates.getSelection();
      Object firstElement = selection.getFirstElement();

      if( firstElement instanceof IFile )
        m_style = (IFile) firstElement;
    }

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

      @SuppressWarnings("synthetic-access")
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        final Object element = selection.getFirstElement();
        if( element instanceof IFile )
          m_style = (IFile) element;
      }
    } );

    /* edit button */
    m_editStyleButton = new Button( parent, SWT.FLAT );
    final String tooltipEdit = "Klicken Sie hier, um den selektierten Style zu ändern oder einen neuen Style zu erstellen.";
    m_editStyleButton.setToolTipText( tooltipEdit );

    m_editStyleButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        if( m_style != null )
        {
          EditStyleDialog styleEditor = new EditStyleDialog( m_editStyleButton.getShell(), m_style );
          styleEditor.open();
        }
      }
    } );

    /* remove button */
    m_removeLineStyleButton = new Button( parent, SWT.FLAT );
    final String tooltipRemove = "Klicken Sie hier, um den selektierten Style zu löschen.";
    m_removeLineStyleButton.setToolTipText( tooltipRemove );

    /* Images for buttons */
    // final Image editImage = KalypsoModel1D2DUIImages.ID_EDIT.createImage();
    final Image editImage = Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_VIEWER_EDIT ).createImage();
    m_editStyleButton.setImage( editImage );

    final Image removeImage = Kalypso1d2dProjectPlugin.getImageProvider().getImageDescriptor( DESCRIPTORS.RESULT_VIEWER_REMOVE ).createImage();
    m_removeLineStyleButton.setImage( removeImage );

  }

  private IFile[] getStyleTemplates( final String path, IFolder scenarioFolder )
  {
    final IFolder stylesFolder = KalypsoModel1D2DHelper.getStylesFolder( scenarioFolder );
    IFolder folder = stylesFolder.getFolder( path );
    return ResourceUtilities.getChildrenQuiet( folder, IResource.DEPTH_ONE );
  }

  public void setEnabled( boolean status )
  {
    m_combStyleTemplates.getControl().setEnabled( status );
    m_editStyleButton.setEnabled( status );
    m_removeLineStyleButton.setEnabled( status );
  }

  public IFile getSelectedStyle( )
  {
    return m_style;
  }

}
