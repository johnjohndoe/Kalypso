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
package org.kalypso.ui.wizard.feature;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Wizard-Page zur Eingabe der Steuerparameter
 * 
 * @author belger
 */
public class FeaturePage extends WizardPage
{
  private final Collection m_changes = new ArrayList();

  private FeatureComposite m_featureComposite;

  private boolean m_overrideCanFlipToNextPage;

  private Feature m_feature;

  private final GMLWorkspace m_workspace;

  private final IFeatureSelectionManager m_selectionManager;

  public FeaturePage( final String pagename, final String title, final ImageDescriptor image,
      final boolean overrideCanFlipToNextPage, final GMLWorkspace workspace, final Feature feature,
      final IFeatureSelectionManager selectionManager )
  {
    super( pagename, title, image );

    m_overrideCanFlipToNextPage = overrideCanFlipToNextPage;
    m_workspace = workspace;
    m_feature = feature;
    m_selectionManager = selectionManager;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    group.setText( getTitle() );

    m_featureComposite = new FeatureComposite( m_workspace, null, m_selectionManager, new URL[] {} );
    m_featureComposite.setFeature( m_workspace, m_feature );
    m_featureComposite.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final FeatureChange change )
      {
        applyFeatureChange( change );
      }

      public void openFeatureRequested( final Feature feature, final FeatureTypeProperty ftp )
      {
      // TODO: open modal dialog?
      }
    } );

    final Control control = m_featureComposite.createControl( group, SWT.NONE );
    control.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    setControl( group );
  }

  /**
   * Add the change to our list of changes. Subclass may override (and call super.applyFeatureChange())
   */
  protected void applyFeatureChange( final FeatureChange change )
  {
    m_changes.add( change );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    if( m_featureComposite != null )
      m_featureComposite.dispose();
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
   */
  public boolean isPageComplete()
  {
    return m_featureComposite.isValid();
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  public boolean canFlipToNextPage()
  {
    if( m_overrideCanFlipToNextPage )
      return super.canFlipToNextPage() && isPageComplete();

    return super.canFlipToNextPage();
  }

  public Collection getChanges()
  {
    return new ArrayList( m_changes );
  }
}