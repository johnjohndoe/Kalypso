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
/*
 * Created on 12.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleFilterCollection;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.model.feature.FeatureType;

/**
 * @author F.Lindemann
 *  
 */
public class RuleTabItemBuilder
{

  private Composite globalComposite = null;

  private Composite tabFolderComposite = null;

  private TabFolder ruleTabFolder = null;

  private KalypsoUserStyle userStyle = null;

  private FeatureType featureType = null;

  private int focusedRuleItem = -1;

  private RuleFilterCollection rulePatternCollection = null;

  private ArrayList numericFeatureTypePropertylist = null;

  public RuleTabItemBuilder( Composite composite, RuleFilterCollection m_rulePatternCollection,
      KalypsoUserStyle m_userStyle, final IKalypsoFeatureTheme theme, ArrayList m_numericFeatureTypePropertylist )
  {
    this.userStyle = m_userStyle;
    if( theme != null )
      this.featureType = theme.getFeatureType();
    this.rulePatternCollection = m_rulePatternCollection;
    this.numericFeatureTypePropertylist = m_numericFeatureTypePropertylist;
    globalComposite = new Composite( composite, SWT.NULL );
  }

  public void draw()
  {
    if( tabFolderComposite != null )
      tabFolderComposite.dispose();

    tabFolderComposite = new Composite( globalComposite, SWT.NULL );
    tabFolderComposite.setLayout( new FormLayout() );
    tabFolderComposite.layout();

    // needs to exist otherwise tab-folder expands arbitrarily
    final Composite innerTabFolderComposite = new Composite( tabFolderComposite, SWT.NULL );
    innerTabFolderComposite.layout();

    ruleTabFolder = new TabFolder( innerTabFolderComposite, SWT.NULL );
    FormData RuleTableFolderLData = new FormData();
    RuleTableFolderLData.height = 507;
    RuleTableFolderLData.width = 245;
    RuleTableFolderLData.top = new FormAttachment( 10, 1000, 0 );
    ruleTabFolder.setLayoutData( RuleTableFolderLData );
    ruleTabFolder.setSize( new org.eclipse.swt.graphics.Point( 245, 507 ) );

    ArrayList filteredRules = rulePatternCollection.getFilteredRuleCollection();
    for( int j = 0; j < filteredRules.size(); j++ )
    {
      Object ruleObject = filteredRules.get( j );
      if( ruleObject instanceof Rule )
      {
        new RuleTabItem( ruleTabFolder, userStyle, featureType ).drawRule( (Rule)ruleObject, j );
      }
      else if( ruleObject instanceof RuleCollection )
      {
        new RulePatternTabItem( ruleTabFolder, userStyle, featureType, rulePatternCollection,
            numericFeatureTypePropertylist ).drawPatternRule( (RuleCollection)ruleObject, j );
      }
    }

    if( focusedRuleItem != -1 )
      ruleTabFolder.setSelection( focusedRuleItem );
    if( filteredRules.size() == 0 )
      ruleTabFolder.setVisible( false );

    tabFolderComposite.pack( true );
  }

  public int getSelectedRule()
  {
    if( ruleTabFolder != null )
      return ruleTabFolder.getSelectionIndex();
    return -1;
  }

  public void setSelectedRule( int index )
  {
    ruleTabFolder.setSelection( index );
  }

}