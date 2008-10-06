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
package org.kalypso.ui.views.properties;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author Gernot Belger
 */
public class NamePropertySection extends AbstractPropertySection
{
  private ModifyListener m_nameListener;

  private IKalypsoTheme m_theme = null;

  private Text m_text;

  private CLabel m_typeValueLabel;

  /**
   * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
   */
  @Override
  public void createControls( final Composite parent, final TabbedPropertySheetPage tabbedPropertySheetPage )
  {
    super.createControls( parent, tabbedPropertySheetPage );

    final Composite composite = getWidgetFactory().createFlatFormComposite( parent );

    m_text = getWidgetFactory().createText( composite, "" ); //$NON-NLS-1$
    final FormData textData = new FormData();
    textData.left = new FormAttachment( 0, STANDARD_LABEL_WIDTH );
    textData.right = new FormAttachment( 100, 0 );
    textData.top = new FormAttachment( 0, ITabbedPropertyConstants.VSPACE );
    m_text.setLayoutData( textData );

    final Text text = m_text;
    m_nameListener = new ModifyListener()
    {
      public void modifyText( ModifyEvent arg0 )
      {
        final IPropertySource properties = (IPropertySource) getTheme().getAdapter( IPropertySource.class );
        properties.setPropertyValue( ThemePropertySource.DESC_NAME, text.getText() );
      }
    };

    m_text.addModifyListener( m_nameListener );

    final CLabel labelLabel = getWidgetFactory().createCLabel( composite, "Name:" );
    final FormData labelData = new FormData();
    labelData.left = new FormAttachment( 0, 0 );
    labelData.right = new FormAttachment( m_text, -ITabbedPropertyConstants.HSPACE );
    labelData.top = new FormAttachment( m_text, 0, SWT.CENTER );
    labelLabel.setLayoutData( labelData );

    m_typeValueLabel = getWidgetFactory().createCLabel( composite, "" );
    final FormData typeValueData = new FormData();
    typeValueData.left = new FormAttachment( 0, STANDARD_LABEL_WIDTH );
    typeValueData.right = new FormAttachment( 100, 0 );
    typeValueData.top = new FormAttachment( m_text, ITabbedPropertyConstants.VSPACE );
    m_typeValueLabel.setLayoutData( typeValueData );

    final CLabel typeLabel = getWidgetFactory().createCLabel( composite, "Typ:" );
    final FormData typeLabelData = new FormData();
    typeLabelData.left = new FormAttachment( 0, 0 );
    typeLabelData.right = new FormAttachment( m_typeValueLabel, -ITabbedPropertyConstants.HSPACE );
    typeLabelData.top = new FormAttachment( m_typeValueLabel, 0, SWT.CENTER );
    typeLabel.setLayoutData( typeLabelData );
  }

  /**
   * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput(org.eclipse.ui.IWorkbenchPart,
   *      org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setInput( final IWorkbenchPart part, final ISelection selection )
  {
    super.setInput( part, selection );

    Assert.isTrue( selection instanceof IStructuredSelection );
    Object input = ((IStructuredSelection) selection).getFirstElement();
    Assert.isTrue( input instanceof IKalypsoTheme );
    m_theme = (IKalypsoTheme) input;
  }

  /**
   * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
   */
  @Override
  public void refresh( )
  {
    final IPropertySource properties = (IPropertySource) m_theme.getAdapter( IPropertySource.class );
    final String name = (String) properties.getPropertyValue( ThemePropertySource.DESC_NAME );
    final String type = (String) properties.getPropertyValue( ThemePropertySource.DESC_TYPE );

    m_text.removeModifyListener( m_nameListener );
    m_text.setText( name );
    m_text.addModifyListener( m_nameListener );

    m_typeValueLabel.setText( type );
  }

  protected IKalypsoTheme getTheme( )
  {
    return m_theme;
  }

}
