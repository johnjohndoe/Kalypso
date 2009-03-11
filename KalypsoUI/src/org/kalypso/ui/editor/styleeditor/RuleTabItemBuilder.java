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
import java.util.Date;
import java.util.List;

import javax.swing.event.EventListenerList;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.panels.AddSymbolizerPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleFilterCollection;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree_impl.filterencoding.BoundaryExpression;
import org.kalypsodeegree_impl.filterencoding.ComplexFilter;
import org.kalypsodeegree_impl.filterencoding.PropertyIsBetweenOperation;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.graphics.sld.StyleFactory;

/**
 * @author F.Lindemann
 */
public class RuleTabItemBuilder
{
  public static enum EventType
  {
    RULE_ADDED,
    RULE_REMOVED,
    RULE_BACKWARD,
    RULE_FORWARD,
    PATTERN_ADDED
  }

  private final EventListenerList m_listenerList = new EventListenerList();

  private final Composite m_globalComposite;

  private final KalypsoUserStyle m_userStyle;

  private final IFeatureType m_featureType;

  private final int m_focusedRuleItem = -1;

  private final RuleFilterCollection m_rulePatternCollection;

  private final List<IPropertyType> m_numericFeatureTypePropertylist;

  private Composite m_tabFolderComposite = null;

  private TabFolder m_ruleTabFolder = null;

  private final FeatureTypeStyle m_fts;

  private final FormToolkit m_toolkit;

  public RuleTabItemBuilder( final FormToolkit toolkit, final Composite parent, final RuleFilterCollection rulePatternCollection, final KalypsoUserStyle userStyle, final IFeatureType featureType, final List<IPropertyType> numericFeatureTypePropertylist )
  {
    m_toolkit = toolkit;
    m_userStyle = userStyle;
    m_fts = m_userStyle.getFeatureTypeStyles()[0];
    m_featureType = featureType;
    m_rulePatternCollection = rulePatternCollection;
    m_numericFeatureTypePropertylist = numericFeatureTypePropertylist;
    m_globalComposite = toolkit.createComposite( parent );
    final GridLayout globalLayout = new GridLayout( 2, false );
    globalLayout.horizontalSpacing = 0;
    globalLayout.marginHeight = 0;
    globalLayout.marginWidth = 0;
    globalLayout.verticalSpacing = 0;
    m_globalComposite.setLayout( globalLayout );
    m_globalComposite.addDisposeListener( new DisposeListener()
    {
      @Override
      public void widgetDisposed( final DisposeEvent e )
      {
        dispose();
      }
    } );

    m_tabFolderComposite = toolkit.createComposite( m_globalComposite );
    m_tabFolderComposite.setBackground( parent.getDisplay().getSystemColor( SWT.COLOR_DARK_GREEN ) );
    final GridLayout tabLayout = new GridLayout();
    tabLayout.horizontalSpacing = 0;
    tabLayout.marginHeight = 0;
    tabLayout.marginWidth = 0;
    tabLayout.verticalSpacing = 0;
    m_tabFolderComposite.setLayout( tabLayout );
    m_tabFolderComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Tool-Bar */
    final ToolBar toolBar = new ToolBar( m_globalComposite, SWT.FLAT | SWT.VERTICAL );
    m_toolkit.adapt( toolBar );
    toolBar.setLayoutData( new GridData( SWT.CENTER, SWT.BEGINNING, false, false ) );
    createActions( toolBar );
    updateActions();
  }

  protected void dispose( )
  {
    final PanelListener[] listeners = m_listenerList.getListeners( PanelListener.class );
    for( final PanelListener panelListener : listeners )
      m_listenerList.remove( PanelListener.class, panelListener );
  }

  public Composite getControl( )
  {
    return m_globalComposite;
  }

  private void createActions( final ToolBar toolBar )
  {
    final ToolBarManager manager = new ToolBarManager( toolBar );

    manager.add( m_addRuleAction );
    m_addRuleAction.setToolTipText( MessageBundle.STYLE_EDITOR_ADD_RULE );

    manager.add( m_addPatternAction );
    m_addPatternAction.setToolTipText( MessageBundle.STYLE_EDITOR_ADD_RULE_PATTERN );

    manager.add( m_removeRuleAction );
    m_removeRuleAction.setToolTipText( MessageBundle.STYLE_EDITOR_REMOVE_RULE );

    manager.add( m_backwardAction );
    m_backwardAction.setToolTipText( MessageBundle.STYLE_EDITOR_BACKWARD );

    manager.add( m_forwardAction );
    m_forwardAction.setToolTipText( MessageBundle.STYLE_EDITOR_FORWARD );

    manager.update( true );
  }

  private void updateActions( )
  {
    m_removeRuleAction.setEnabled( m_rulePatternCollection.size() > 0 );
    m_addPatternAction.setEnabled( m_numericFeatureTypePropertylist.size() > 0 );
    m_backwardAction.setEnabled( m_rulePatternCollection.size() > 1 );
    m_forwardAction.setEnabled( m_rulePatternCollection.size() > 1 );
  }

  public void addPanelListener( final PanelListener pl )
  {
    m_listenerList.add( PanelListener.class, pl );
  }

  private void fireRulesChanged( final EventType eventType, final Object param )
  {
    final PanelListener[] listeners = m_listenerList.getListeners( PanelListener.class );
    final PanelEvent event = new PanelEvent( this, eventType, param );
    for( final PanelListener panelListener : listeners )
      panelListener.valueChanged( event );
  }

  public void draw( )
  {
    /* Reset */
    final Control[] children = m_tabFolderComposite.getChildren();
    for( final Control control : children )
      control.dispose();

    /* Tab-Folder */
    m_ruleTabFolder = new TabFolder( m_tabFolderComposite, SWT.NULL );
    m_toolkit.adapt( m_ruleTabFolder );
    m_ruleTabFolder.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_ruleTabFolder.setSize( new org.eclipse.swt.graphics.Point( 245, 507 ) );

    /* One tab per rule */
    final List< ? > filteredRules = m_rulePatternCollection.getFilteredRuleCollection();
    for( int j = 0; j < filteredRules.size(); j++ )
    {
      final Object ruleObject = filteredRules.get( j );
      if( ruleObject instanceof Rule )
        new RuleTabItem( m_toolkit, m_ruleTabFolder, m_userStyle, m_featureType ).drawRule( (Rule) ruleObject, j );
      else if( ruleObject instanceof RuleCollection )
        new RulePatternTabItem( m_toolkit, m_ruleTabFolder, m_userStyle, m_featureType, m_rulePatternCollection, m_numericFeatureTypePropertylist ).drawPatternRule( (RuleCollection) ruleObject, j );
    }

    if( m_focusedRuleItem != -1 )
      m_ruleTabFolder.setSelection( m_focusedRuleItem );
    if( filteredRules.size() == 0 )
      m_ruleTabFolder.setVisible( false );

    m_tabFolderComposite.pack( true );
  }

  public int getSelectedRule( )
  {
    if( m_ruleTabFolder != null )
      return m_ruleTabFolder.getSelectionIndex();
    return -1;
  }

  public void setSelectedRule( final int index )
  {
    m_ruleTabFolder.setSelection( index );
  }

  private final IAction m_addRuleAction = new Action( "Add rule", ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE )
  {
    // ADD_RULE
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      handleAddRule();
    }
  };

  private final IAction m_removeRuleAction = new Action( "Remove rule", ImageProvider.IMAGE_STYLEEDITOR_REMOVE )
  {
    // REM_RULE
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      handleRemoveRule();
    }
  };

  private final IAction m_addPatternAction = new Action( "Add pattern rule", ImageProvider.IMAGE_STYLEEDITOR_ADD_RULE_PATTERN )
  {
    // ADD_PATTERN_RULE
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      handleAddPattern();
    }
  };

  private final IAction m_backwardAction = new Action( "Move backwards", ImageProvider.IMAGE_STYLEEDITOR_BACKWARD )
  {
    // BAK_RULE
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      handleBackwardsRule();
    }
  };

  private final IAction m_forwardAction = new Action( "Move forwards", ImageProvider.IMAGE_STYLEEDITOR_FORWARD )
  {
    // FOR_RULE
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      handleForwardRule();
    }
  };

  protected void handleAddRule( )
  {
    final Rule rule = StyleFactory.createRule( (Symbolizer[]) null );
    m_fts.addRule( rule );

    fireRulesChanged( EventType.RULE_ADDED, m_rulePatternCollection.size() );
  }

  protected void handleForwardRule( )
  {
    final int index = getSelectedRule();
    if( index == m_rulePatternCollection.size() - 1 || index < 0 )
      return;

    m_rulePatternCollection.moveForward( index );
    fireRulesChanged( EventType.RULE_FORWARD, index + 1 );
  }

  protected void handleBackwardsRule( )
  {
    final int index = getSelectedRule();
    if( index > 0 )
    {
      m_rulePatternCollection.moveBackward( index );
      fireRulesChanged( EventType.RULE_BACKWARD, index - 1 );
    }
  }

  protected void handleAddPattern( )
  {
    // create a pattern-filter for this style
    if( m_numericFeatureTypePropertylist.isEmpty() )
      return;

    // set by default first featuretypeproperty
    final IPropertyType prop = m_numericFeatureTypePropertylist.get( 0 );
    final PropertyName propertyName = new PropertyName( prop.getName() );

    final List< ? > geometryObjects = AddSymbolizerPanel.queryGeometriesPropertyNames( m_featureType.getProperties(), null );
    // geometryObjects = AddSymbolizerPanel.queryGeometriesPropertyNames(
    // getFeatureType().getVirtuelFeatureTypeProperty(),geometryObjects );

    if( geometryObjects.size() > 0 )
    {
      final Symbolizer symbo = AddSymbolizerPanel.getSymbolizer( new PropertyName( (String) geometryObjects.get( 0 ) ), "Point", m_featureType ); //$NON-NLS-1$
      final String patternName = "-name-" + new Date().getTime(); //$NON-NLS-1$
      final BoundaryExpression upperBoundary = new BoundaryExpression( "1" );
      final BoundaryExpression lowerBoundary = new BoundaryExpression( "0" );
      final PropertyIsBetweenOperation operation = new PropertyIsBetweenOperation( propertyName, lowerBoundary, upperBoundary );
      final ArrayList<Rule> ruleList = new ArrayList<Rule>();
      ruleList.add( StyleFactory.createRule( null, patternName, "", "abstract", null, new ComplexFilter( operation ), false, symbo.getMinScaleDenominator(), symbo.getMaxScaleDenominator() ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_fts.addRule( StyleFactory.createRule( null, patternName, "", "abstract", null, new ComplexFilter( operation ), false, symbo.getMinScaleDenominator(), symbo.getMaxScaleDenominator() ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    fireRulesChanged( EventType.PATTERN_ADDED, m_rulePatternCollection.size() );
  }

  protected void handleRemoveRule( )
  {
    final int index2 = getSelectedRule();
    if( index2 > -1 && index2 < m_rulePatternCollection.size() )
    {
      final Object ruleObject = m_rulePatternCollection.getFilteredRuleCollection().get( index2 );
      if( ruleObject instanceof Rule )
      {
        m_fts.removeRule( (Rule) ruleObject );
      }
      else if( ruleObject instanceof RuleCollection )
      {
        final RuleCollection ruleCollection = (RuleCollection) ruleObject;
        for( int i = 0; i < ruleCollection.size(); i++ )
          m_fts.removeRule( ruleCollection.get( i ) );
      }

      final Integer index2select = index2 > 0 ? null : index2 - 1;
      fireRulesChanged( EventType.RULE_REMOVED, index2select );
    }
  }

}