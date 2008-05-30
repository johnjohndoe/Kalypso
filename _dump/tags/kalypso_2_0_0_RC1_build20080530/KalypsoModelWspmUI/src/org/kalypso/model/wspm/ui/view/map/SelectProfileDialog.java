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
package org.kalypso.model.wspm.ui.view.map;

import java.math.BigDecimal;

import javax.xml.namespace.QName;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.xml.NS;
import org.kalypso.ogc.gml.FeatureUtils;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypsodeegree.model.feature.Feature;

public class SelectProfileDialog extends TitleAreaDialog
{
  protected EasyFeatureWrapper m_selection = null;

  private final EasyFeatureWrapper[] m_crossSections;

  public SelectProfileDialog( final Shell parentShell, final EasyFeatureWrapper[] crossSections )
  {
    super( parentShell );
    m_crossSections = crossSections;
    setBlockOnOpen( true );
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parent )
  {
    final Control contents = super.createContents( parent );

    setTitle( "Select Cross-Sectional Profile" );
    setMessage( "Select a single Cross-Sectional Profile from the lower box." );

    return contents;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = (Composite) super.createDialogArea( parent );
    composite.setLayout( new GridLayout() );
    final GridData data = new GridData( GridData.FILL, GridData.FILL, true, true );
    data.heightHint = 300;
    data.widthHint = 100;

    composite.setLayoutData( data );

    final Label label = new Label( composite, SWT.NONE );
    label.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    label.setText( "Cross-Sectional Profile" );

    final ComboViewer viewer = new ComboViewer( composite, SWT.READ_ONLY | SWT.BORDER );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof EasyFeatureWrapper )
        {
          final EasyFeatureWrapper eft = (EasyFeatureWrapper) element;

          final Feature crossSection = eft.getFeature();

          final String name = FeatureUtils.getFeatureName( NS.GML3, crossSection );
          final QName qStation = new QName( "org.kalypso.model.wspmprofile", "station" ); //$NON-NLS-1$ //$NON-NLS-2$
          final BigDecimal station = (BigDecimal) crossSection.getProperty( qStation );

          final Feature waterbody = crossSection.getParent();

          final String wn = FeatureUtils.getFeatureName( NS.GML3, waterbody );
          final String s = String.format( "%s, Station: %f - Waterbody: %s", name, station, wn );

          return s;
        }

        return super.getText( element );
      }
    } );

    viewer.setInput( m_crossSections );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final StructuredSelection selection = (StructuredSelection) viewer.getSelection();
        m_selection = (EasyFeatureWrapper) selection.getFirstElement();
      }
    } );

    return composite;
  }

  public EasyFeatureWrapper[] getSelectedCrossSection( )
  {
    if( m_selection == null )
      return new EasyFeatureWrapper[] {};

    return new EasyFeatureWrapper[] { m_selection };
  }
}
