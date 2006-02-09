package org.kalypso.ogc.gml.featureview.dialog;

import java.rmi.RemoteException;
import java.util.Collection;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

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

public class RectifiedGridDomainFeatureDialog implements IFeatureDialog
{

  private final Feature m_feature;

  private final IPropertyType m_ftp;

  public RectifiedGridDomainFeatureDialog( final Feature feature, final IPropertyType ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#open(org.eclipse.swt.widgets.Shell)
   */
  public int open( Shell shell )
  {
    GridDialog gridDialog = new GridDialog( shell );
    final int open = gridDialog.open();
    if( open == Window.OK )
    {
      //System.out.println("Window.OK");
    }
    return open;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( Collection c )
  {
  //no changes
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#getLabel()
   */
  public String getLabel()
  {
    return "...";
  }

  public RectifiedGridDomain getRectifiedGridDomain()
  {
    return (RectifiedGridDomain)m_feature.getProperty( m_ftp.getName() );
  }

  class GridDialog extends Dialog
  {

    public GridDialog( Shell parentShell )
    {
      super( parentShell );
      setShellStyle( getShellStyle() | SWT.RESIZE );
    }

    protected Control createDialogArea( final Composite parent )
    {
      getShell().setText( "RectifiedGridDomain properties" );

      final ScrolledComposite scrolledComposite = new ScrolledComposite( parent, SWT.H_SCROLL | SWT.V_SCROLL
          | SWT.BORDER );

      // don't forget this line!
      scrolledComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

      Composite control = null;
      try
      {
        control = createControl( scrolledComposite, SWT.NONE );
      }
      catch( RemoteException e )
      {
        e.printStackTrace();
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
      control.setSize( control.computeSize( SWT.DEFAULT, SWT.DEFAULT ) );
      scrolledComposite.setContent( control );

      return scrolledComposite;
    }

    private Composite createControl( Composite scrolledComposite, int style ) throws RemoteException, Exception
    {

      Composite mainComposite = new Composite( scrolledComposite, style );
      GridLayout compositeLayout = new GridLayout();
      compositeLayout.numColumns = 1;
      compositeLayout.marginWidth = 10;
      compositeLayout.marginHeight = 5;
      compositeLayout.verticalSpacing = 5;
      mainComposite.setLayout( compositeLayout );
      RectifiedGridDomain gridDomain = getRectifiedGridDomain();

      Group originGroup = new Group( mainComposite, style );
      GridLayout originLayout = new GridLayout();
      originLayout.numColumns = 2;
      originGroup.setLayout( originLayout );
      originGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_BOTH ) );
      originGroup.setText( "Origin" );
      Label cooLabel = new Label( originGroup, SWT.NONE );
      cooLabel.setText( "CoordinateSystem: " );
      Label cooValueLabel = new Label( originGroup, SWT.NONE );
      cooValueLabel.setText( gridDomain.getOrigin( null ).getCoordinateSystem().getName() );
      Label xLabel = new Label( originGroup, SWT.NONE );
      xLabel.setText( "Coordinate X: " );
      Label xValueLabel = new Label( originGroup, SWT.NONE );
      xValueLabel.setText( gridDomain.getOrigin( null ).getX() + "" );
      Label yLabel = new Label( originGroup, SWT.NONE );
      yLabel.setText( "Coordinate Y: " );
      Label yValueLabel = new Label( originGroup, SWT.NONE );
      yValueLabel.setText( gridDomain.getOrigin( null ).getY() + "" );

      Group gridRangeGroup = new Group( mainComposite, style );
      GridLayout gridRangeLayout = new GridLayout();
      gridRangeLayout.numColumns = 2;
      gridRangeGroup.setLayout( gridRangeLayout );
      gridRangeGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_BOTH ) );
      gridRangeGroup.setText( "GridRange" );
      Label colLabel = new Label( gridRangeGroup, SWT.NONE );
      colLabel.setText( "Number of columns: " );
      Label colValueLabel = new Label( gridRangeGroup, SWT.NONE );
      colValueLabel.setText( gridDomain.getNumColumns() + "" );
      Label rowLabel = new Label( gridRangeGroup, SWT.NONE );
      rowLabel.setText( "Number of rows: " );
      Label rowValueLabel = new Label( gridRangeGroup, SWT.NONE );
      rowValueLabel.setText( gridDomain.getNumRows() + "" );

      Group offsetGroup = new Group( mainComposite, style );
      GridLayout offsetLayout = new GridLayout();
      offsetLayout.numColumns = 2;
      offsetGroup.setLayout( offsetLayout );
      offsetGroup.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.FILL_BOTH ) );
      offsetGroup.setText( "Offset" );
      Label offsetXLabel = new Label( offsetGroup, SWT.NONE );
      offsetXLabel.setText( "Offset X: " );
      Label offsetXValueLabel = new Label( offsetGroup, SWT.NONE );
      offsetXValueLabel.setText( gridDomain.getOffset()[0] + "" );
      Label offsetYLabel = new Label( offsetGroup, SWT.NONE );
      offsetYLabel.setText( "Offset Y: " );
      Label offsetYValueLabel = new Label( offsetGroup, SWT.NONE );
      offsetYValueLabel.setText( gridDomain.getOffset()[1] + "" );

      return mainComposite;
    }
  }

}