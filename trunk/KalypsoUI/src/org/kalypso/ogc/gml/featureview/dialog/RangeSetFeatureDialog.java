package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.cv.RangeSet;

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

public class RangeSetFeatureDialog implements IFeatureDialog
{

  private final Feature m_feature;

  private final IPropertyType m_ftp;

  public RangeSetFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( Shell shell )
  {
    RangeSetDialog rangeSetDialog = new RangeSetDialog( shell );
    final int open = rangeSetDialog.open();
    return open;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection<FeatureChange> c )
  {
  //  no changes allowed
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    return "..."; //$NON-NLS-1$
  }

  public RangeSet getRangeSet()
  {
    return (RangeSet)m_feature.getProperty( m_ftp.getName() );
  }

  class RangeSetDialog extends Dialog
  {

    public RangeSetDialog( Shell parentShell )
    {
      super( parentShell );
      setShellStyle( getShellStyle() | SWT.RESIZE );
    }

    @Override
    protected Control createDialogArea( final Composite parent )
    {
      getShell().setText( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.RangeSetFeatureDialog.properties") ); //$NON-NLS-1$

      final ScrolledComposite scrolledComposite = new ScrolledComposite( parent, SWT.H_SCROLL | SWT.V_SCROLL
          | SWT.BORDER );

      GridData gridData = new GridData( GridData.FILL_BOTH );
      gridData.grabExcessHorizontalSpace = true;
      gridData.grabExcessVerticalSpace = true;
      // don't forget this line!
      scrolledComposite.setLayoutData( gridData );

      final Composite control = createControl( scrolledComposite, SWT.NONE );
      control.setSize( control.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );
      scrolledComposite.setContent( control );

      return scrolledComposite;
    }

    private Composite createControl( Composite scrolledComposite, int style )
    {
      Composite mainComposite = new Composite( scrolledComposite, style );
      GridLayout gridLayout = new GridLayout();
      gridLayout.numColumns = 2;
      mainComposite.setLayout( gridLayout );

      RangeSet rangeSet = getRangeSet();

      Label minLabel = new Label( mainComposite, SWT.NONE );
      minLabel.setText( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.RangeSetFeatureDialog.min") ); //$NON-NLS-1$
      Label minValueLabel = new Label( mainComposite, SWT.NONE );
      minValueLabel.setText( rangeSet.getMinValue() + "" ); //$NON-NLS-1$

      Label maxLabel = new Label( mainComposite, SWT.NONE );
      maxLabel.setText( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.RangeSetFeatureDialog.max") ); //$NON-NLS-1$
      Label maxValueLabel = new Label( mainComposite, SWT.NONE );
      maxValueLabel.setText( rangeSet.getMaxValue() + "" ); //$NON-NLS-1$

      Label fileLabel = new Label( mainComposite, SWT.NONE );
      fileLabel.setText( Messages.getString("org.kalypso.ogc.gml.featureview.dialog.RangeSetFeatureDialog.file") ); //$NON-NLS-1$
      Label filePathLabel = new Label( mainComposite, SWT.NONE );
      filePathLabel.setText( rangeSet.getRangeSetDataFile() );

      return mainComposite;
    }
  }

}