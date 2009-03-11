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
 * Created on 09.07.2004
 *
 */
package org.kalypso.ui.editor.styleeditor;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.outline.SaveStyleAction;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.styleeditor.RuleTabItemBuilder.EventType;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleCollection;
import org.kalypso.ui.editor.styleeditor.rulePattern.RuleFilterCollection;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.graphics.sld.UserStyle;

/**
 * @author F.Lindemann
 */
public class SLDEditorGuiBuilder
{
  private final Action m_saveAction = new Action( "Save", ImageProvider.IMAGE_STYLEEDITOR_SAVE )
  {
    /**
     * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
     */
    @Override
    public void runWithEvent( final Event event )
    {
      handleSavePressed( event.display.getActiveShell() );
    }
  };

  private final PanelListener m_panelListener = new PanelListener()
  {
    public void valueChanged( final PanelEvent event )
    {
      handleValueChanged( event.eventType, event.param );
    }
  };

  private IFeatureType m_featureType = null;

  private Composite m_parent = null;

  private int m_focusedRuleItem = -1;

  private RuleFilterCollection m_rulePatternCollection = null;

  private final ScrolledForm m_form;

  private KalypsoUserStyle m_userStyle;

  private IKalypsoFeatureTheme m_theme;

  private final FormToolkit m_toolkit;

  public SLDEditorGuiBuilder( final FormToolkit toolkit, final Composite parent )
  {
    m_toolkit = toolkit;
    m_parent = parent;

    m_form = toolkit.createScrolledForm( m_parent );
    m_form.setText( "Style Editor" );
    m_form.getBody().setLayout( new GridLayout() );

    final IToolBarManager toolBarManager = m_form.getForm().getToolBarManager();
    createActions( toolBarManager );
    toolBarManager.update( true );

    setStyle( null, null );
  }

  private void createActions( final IToolBarManager toolBarManager )
  {
    m_saveAction.setToolTipText( MessageBundle.STYLE_EDITOR_SAVE_STYLE );

    toolBarManager.add( m_saveAction );
  }

  protected void handleSavePressed( final Shell shell )
  {
    SaveStyleAction.saveUserStyle( m_userStyle, shell );
  }

  public Composite getControl( )
  {
    return m_form;
  }

  public void setStyle( final KalypsoUserStyle userStyle, final IKalypsoFeatureTheme theme )
  {
    setStyle( userStyle, theme, -1 );
  }

  public void setStyle( final KalypsoUserStyle userStyle, final IKalypsoFeatureTheme theme, final int index )
  {
    // TODO: missing level: FeatureTypeStyle between UserStyle and Rule! Must also be added to outline

    m_theme = theme;
    m_userStyle = userStyle;

    if( index != -1 )
      m_focusedRuleItem = index;

    // get IFeatureType from layer
    if( theme != null )
      m_featureType = theme.getFeatureType();

    /* Rebuild the ui */
    final Composite mainComposite = m_form.getBody();
    final Control[] bodyChildren = mainComposite.getChildren();
    for( final Control control : bodyChildren )
      control.dispose();

    /* Configure actions */
    m_saveAction.setEnabled( m_userStyle != null );

    /* User-Style */
    final String formTitle = userStyle == null ? MessageBundle.STYLE_EDITOR_NO_STYLE_FOR_EDITOR : userStyle.getTitle();
    m_form.setText( formTitle );

    updateRuleTabs( userStyle, mainComposite );

    m_form.reflow( true );
  }

  private void updateRuleTabs( final KalypsoUserStyle userStyle, final Composite mainComposite )
  {
    if( userStyle == null )
      return;

    final Rule[] rules = getRules( userStyle );
    m_rulePatternCollection = new RuleFilterCollection();
    // filter patterns from rules and draw them afterwards
    for( final Rule element : rules )
      m_rulePatternCollection.addRule( element );

    // check whether there are featureTypes that have numeric properties to be
    // used by a pattern-filter
    final List<IPropertyType> numericFeatureTypePropertylist = new ArrayList<IPropertyType>();
    final IPropertyType[] ftp = getFeatureType().getProperties();
    for( final IPropertyType propertyType : ftp )
    {
      if( propertyType instanceof IValuePropertyType )
      {
        final IValuePropertyType vpt = (IValuePropertyType) propertyType;
        final Class< ? > valueClass = vpt.getValueClass();
        if( Number.class.isAssignableFrom( valueClass ) )
          numericFeatureTypePropertylist.add( propertyType );
      }
    }

    final RuleTabItemBuilder ruleTabItemBuilder = new RuleTabItemBuilder( m_toolkit, mainComposite, m_rulePatternCollection, userStyle, m_featureType, numericFeatureTypePropertylist );
    ruleTabItemBuilder.addPanelListener( m_panelListener );
    ruleTabItemBuilder.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    ruleTabItemBuilder.draw();
    if( m_focusedRuleItem > -1 )
      ruleTabItemBuilder.setSelectedRule( m_focusedRuleItem );
  }

  private Rule[] getRules( final UserStyle style )
  {
    final FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    return fts[0].getRules();
  }

  void setRules( final List< ? > ruleObjects, final UserStyle style )
  {
    final FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    final ArrayList<Rule> ruleInstances = new ArrayList<Rule>();
    for( int i = 0; i < ruleObjects.size(); i++ )
    {
      final Object object = ruleObjects.get( i );
      if( object instanceof Rule )
        ruleInstances.add( (Rule) object );
      else if( object instanceof RuleCollection )
      {
        final RuleCollection ruleCollection = (RuleCollection) object;
        for( int j = 0; j < ruleCollection.size(); j++ )
          ruleInstances.add( ruleCollection.get( j ) );
      }
    }
    final Rule[] ruleArray = new Rule[ruleInstances.size()];
    for( int c = 0; c < ruleInstances.size(); c++ )
      ruleArray[c] = ruleInstances.get( c );
    fts[0].setRules( ruleArray );
  }

  void addRule( final Rule rule, final UserStyle style )
  {
    final FeatureTypeStyle fts[] = style.getFeatureTypeStyles();
    fts[0].addRule( rule );
  }

  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  public void setFocus( )
  {
    m_form.setFocus();
  }

  protected void handleValueChanged( final EventType eventType, final Object param )
  {
    switch( eventType )
    {
      case RULE_ADDED:
        break;

      case RULE_REMOVED:
        break;

      case RULE_BACKWARD:
        break;

      case RULE_FORWARD:
        break;

      case PATTERN_ADDED:
        break;
    }

    if( param instanceof Integer )
      this.m_focusedRuleItem = ((Integer) param);
    m_userStyle.fireStyleChanged();
    setStyle( m_userStyle, m_theme );
  }

}