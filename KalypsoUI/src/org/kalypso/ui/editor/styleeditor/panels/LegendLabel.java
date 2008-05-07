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
package org.kalypso.ui.editor.styleeditor.panels;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.contribs.eclipse.swt.awt.ImageConverter;
import org.kalypso.ogc.gml.IKalypsoUserStyleListener;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypsodeegree.graphics.legend.LegendElement;
import org.kalypsodeegree.graphics.legend.LegendElementCollection;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.graphics.legend.LegendElementCollection_Impl;
import org.kalypsodeegree_impl.graphics.legend.LegendFactory;
import org.kalypsodeegree_impl.graphics.sld.FeatureTypeStyle_Impl;
import org.kalypsodeegree_impl.graphics.sld.UserStyle_Impl;

/**
 * @author F.Lindemann
 */
public class LegendLabel implements DisposeListener, IKalypsoUserStyleListener
{
  private Label label = null;

  private KalypsoUserStyle userStyle = null;

  private int ruleIndex = -1;

  private Composite composite = null;

  public LegendLabel( final Composite parent, final KalypsoUserStyle m_userStyle )
  {
    new LegendLabel( parent, m_userStyle, -1 );
  }

  public LegendLabel( final Composite parent, final KalypsoUserStyle m_userStyle, final int i )
  {
    composite = new Composite( parent, SWT.NULL );
    final FormLayout compositeLayout = new FormLayout();
    final GridData compositeData = new GridData();
    compositeData.widthHint = 180;
    composite.setLayoutData( compositeData );
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    compositeLayout.spacing = 0;
    composite.layout();

    final Label legendLabel = new Label( composite, SWT.NULL );
    final FormData legendLabelData = new FormData();
    legendLabelData.height = 15;
    legendLabelData.width = 46;
    legendLabelData.left = new FormAttachment( 0, 1000, 0 );
    legendLabelData.top = new FormAttachment( 150, 1000, 0 );
    legendLabel.setLayoutData( legendLabelData );
    legendLabel.setText( MessageBundle.STYLE_EDITOR_LEGEND );

    setLabel( new Label( composite, SWT.NULL ) );
    final FormData labelData = new FormData();
    labelData.left = new FormAttachment( 340, 1000, 0 );
    labelData.top = new FormAttachment( 0, 1000, 0 );
    labelData.width = 41;
    getLabel().setLayoutData( labelData );

    setRuleIndex( i );
    setUserStyle( m_userStyle );
    getLabel().addDisposeListener( this );
    setLegendImage( getLabel(), m_userStyle );
    m_userStyle.addStyleListener( this );
  }

  private void setLegendImage( final Label m_label, final UserStyle m_userStyle )
  {
    final LegendFactory factory = new LegendFactory();
    try
    {
      LegendElement le = null;

      if( ruleIndex != -1 && ruleIndex < m_userStyle.getFeatureTypeStyles()[0].getRules().length )
      {
        // NECESSARY IF TO SHOW STYLE OF ONLY ONE RULE
        final FeatureTypeStyle_Impl fts = new FeatureTypeStyle_Impl();
        final Rule rule = m_userStyle.getFeatureTypeStyles()[0].getRules()[ruleIndex];
        final Rule m_rules[] = { rule };
        fts.setRules( m_rules );
        final FeatureTypeStyle_Impl[] ftStyles = { fts };
        final UserStyle ruleStyle = new UserStyle_Impl( null, null, null, true, ftStyles );
        le = factory.createLegendElement( ruleStyle, 40, 20, "" ); //$NON-NLS-1$
        // This is necessary, as I don't want title of the filter to appear in
        // the label but only
        // an image of the filter itself
        if( le instanceof LegendElementCollection )
        {
          final LegendElement elements[] = ((LegendElementCollection_Impl) le).getLegendElements();
          if( elements.length > 0 )
            le = elements[0];
        }
      }
      else
      {
        le = factory.createLegendElement( m_userStyle, 40, 20, "" ); //$NON-NLS-1$
      }

      if( le == null )
        return;

      final BufferedImage bi = le.exportAsImage();
      final BufferedImage outbi = new BufferedImage( 40, 20, BufferedImage.TYPE_INT_ARGB );
      final Graphics g = outbi.getGraphics();
      g.drawImage( bi, 0, 0, Color.WHITE, null );

      // TODO Is the swt image disposed somewhere?
      ImageData img = ImageConverter.convertToSWT( outbi );
      Image swtImage = ImageDescriptor.createFromImageData( img ).createImage();

      m_label.setImage( swtImage );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
    }
  }

  public void widgetDisposed( final DisposeEvent e )
  {
    getUserStyle().removeStyleListener( this );
  }

  public Label getLabel( )
  {
    return label;
  }

  public void setLabel( final Label m_label )
  {
    this.label = m_label;
  }

  public int getRuleIndex( )
  {
    return ruleIndex;
  }

  public void setRuleIndex( final int m_ruleIndex )
  {
    this.ruleIndex = m_ruleIndex;
  }

  public KalypsoUserStyle getUserStyle( )
  {
    return userStyle;
  }

  public void setUserStyle( final KalypsoUserStyle m_userStyle )
  {
    this.userStyle = m_userStyle;
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoUserStyleListener#styleChanged(org.kalypso.ogc.gml.KalypsoUserStyle)
   */
  public void styleChanged( final KalypsoUserStyle source )
  {
    setLegendImage( getLabel(), source );
  }
}