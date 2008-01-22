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
package org.kalypso.util.swt;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;

/**
 * A composite, showing an {@link org.eclipse.core.runtime.IStatus}.<br> *
 * <dl>
 * <dt><b>Styles:</b></dt>
 * <dd>DETAILS</dd>
 * <dt><b>Events:</b></dt>
 * <dd>(none)</dd>
 * </dl>
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class StatusComposite extends Composite
{
  /**
   * Style constant: if set, a details button is shown.
   */
  public static final int DETAILS = SWT.SEARCH;

  private final Label m_statusImgLabel;

  private final Label m_statusMessageLabel;

  private IStatus m_status;

  private final Button m_detailsButton;

  public StatusComposite( final Composite parent, final int style )
  {
    super( parent, style );

    final GridLayout gridLayout = new GridLayout( 3, false );
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;

    super.setLayout( gridLayout );

    m_statusImgLabel = new Label( this, SWT.NONE );
    m_statusImgLabel.setLayoutData( new GridData( SWT.CENTER, SWT.CENTER, false, false ) );
    m_statusMessageLabel = new Label( this, SWT.NONE );
    m_statusMessageLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    if( (style & DETAILS) != 0 )
    {
      m_detailsButton = new Button( this, SWT.PUSH );
      m_detailsButton.setText( "&Details" );
      m_detailsButton.addSelectionListener( new SelectionAdapter()
      {
        /**
         * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
         */
        @Override
        public void widgetSelected( final SelectionEvent e )
        {
          detailsButtonPressed();
        }
      } );
    }
    else
      m_detailsButton = null;
  }

  protected void detailsButtonPressed( )
  {
    final StatusDialog statusTableDialog = new StatusDialog( getShell(), m_status, "Details" );
    statusTableDialog.open();
  }

  /**
   * @see org.eclipse.swt.widgets.Composite#setLayout(org.eclipse.swt.widgets.Layout)
   */
  @Override
  public void setLayout( final Layout layout )
  {
    throw new UnsupportedOperationException( "The layout of this composite is fixed." );
  }

  public void setStatus( final IStatus status )
  {
    m_status = status;

    if( isDisposed() )
      return;

    if( status == null )
    {
      m_statusImgLabel.setImage( null );
      m_statusMessageLabel.setText( "" );
      if( m_detailsButton != null )
        m_detailsButton.setEnabled( false );
    }
    else
    {
      m_statusImgLabel.setImage( getStatusImage( status ) );
      m_statusMessageLabel.setText( status.getMessage() );
      if( m_detailsButton != null )
        m_detailsButton.setEnabled( status.isMultiStatus() || status.getException() != null );
    }

    layout();
  }

  // TODO: move both methods into contribution plug-ins

  /**
   * Get the IDE image at path.
   * 
   * @param path
   * @return Image
   */
  public static Image getIDEImage( final String constantName )
  {
    return JFaceResources.getResources().createImageWithDefault( IDEInternalWorkbenchImages.getImageDescriptor( constantName ) );
  }

  @SuppressWarnings("restriction")
  public static Image getStatusImage( final IStatus status )
  {
    switch( status.getSeverity() )
    {
      case IGeoStatus.OK:
        return KalypsoGisPlugin.getImageProvider().getImage( ImageProvider.DESCRIPTORS.STATUS_IMAGE_OK );

        // case IGeoStatus.CANCEL:
        // return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_INCOMPLETE_TSK );

      case IGeoStatus.ERROR:
        return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_ERROR_PATH );

      case IGeoStatus.WARNING:
        return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH );

      case IGeoStatus.INFO:
        return getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_INFO_PATH );

      default:
        return null;
    }
  }

}
