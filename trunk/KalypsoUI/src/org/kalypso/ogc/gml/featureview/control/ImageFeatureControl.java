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
package org.kalypso.ogc.gml.featureview.control;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class ImageFeatureControl extends AbstractFeatureControl
{
  private Label m_label;

  private static final QName QNAME_STRING = new QName( XMLConstants.W3C_XPATH_DATATYPE_NS_URI, "string" ); //$NON-NLS-1$

  public ImageFeatureControl( final IPropertyType ftp )
  {
    super( ftp );
  }

  public ImageFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_label = new Label( parent, style );

    updateControl();

    return m_label;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    // this control does not modify, so its always valid
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    // this control does not modify, so listeners are ignored
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    // this control does not modify, so listeners are ignored
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    final Image oldImage = m_label.getImage();
    if( oldImage != null )
      oldImage.dispose();
    m_label.setImage( null );
    m_label.setToolTipText( null );

    final Feature feature = getFeature();
    final IPropertyType pt = getFeatureTypeProperty();

    // must be a string property
    final String imgPath;
    if( feature == null || pt == null || GMLSchemaUtilities.substitutes( feature.getFeatureType(), QNAME_STRING ) )
      imgPath = null;
    else
    {
      final Object uriString = feature.getProperty( pt );
      imgPath = uriString == null ? "" : (String) uriString; //$NON-NLS-1$
    }

    String problemMessage = null;
    Image image = null;
    String tooltip = null;

    if( imgPath == null )
      problemMessage = Messages.getString("org.kalypso.ogc.gml.featureview.control.ImageFeatureControl.2") + pt; //$NON-NLS-1$
    else if( imgPath.length() == 0 )
      problemMessage = ""; //$NON-NLS-1$
    else
    {
      final GMLWorkspace workspace = feature.getWorkspace();
      final URL context = workspace.getContext();
      try
      {
        final URL url = new URL( context, imgPath );
        final ImageDescriptor imgDesc = ImageDescriptor.createFromURL( url );
        image = imgDesc.createImage( false );
        if( image == null )
          problemMessage = Messages.getString("org.kalypso.ogc.gml.featureview.control.ImageFeatureControl.4") + url.toExternalForm(); //$NON-NLS-1$
        else
          tooltip = url.toExternalForm();
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();

        problemMessage = Messages.getString("org.kalypso.ogc.gml.featureview.control.ImageFeatureControl.5") + imgPath; //$NON-NLS-1$
      }
    }

    if( image == null )
    {
      m_label.setText( problemMessage );
      // SAD: label is not able to display image AND text
      // m_label.setImage( JFaceResources.getImage( Dialog.DLG_IMG_MESSAGE_WARNING ) );
    }
    else
    {
      m_label.setImage( image );
      m_label.setToolTipText( tooltip );
    }

    layoutScrolledParent( m_label );
  }

  public static void layoutScrolledParent( final Control control )
  {
    if( control == null )
      return;

    final Composite parent = control.getParent();
    if( parent == null )
      return;

    if( parent instanceof ScrolledComposite )
    {
      parent.layout();
      return;
    }

    parent.layout();
    layoutScrolledParent( parent );
  }
}
