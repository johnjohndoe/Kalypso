package org.kalypso.kalypso1d2d.pjt.actions;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.RoughnessPolygon;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.command.ActivateThemeCommand;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.EnableThemeCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;
import org.kalypso.ui.views.map.MapView;

public class SelectRoughnessThemeHandler extends AbstractHandler
{
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    final Shell shell = (Shell) context.getVariable( ISources.ACTIVE_SHELL_NAME );

    /* Get the map */
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.SelectRoughnessThemeHandler.0" ) ); //$NON-NLS-1$

    final IMapPanel mapPanel = mapView.getMapPanel();
    IMapModell orgMapModell = mapPanel.getMapModell();

    synchronized( this )
    {
      while( orgMapModell == null )
        try
      {
          this.wait( 500 );
          orgMapModell = mapPanel.getMapModell();
          // TODO: could not be right, throws NPE and leaves the while loop
          // orgMapModell.getAllThemes();
      }
      catch( final InterruptedException e )
      {
        e.printStackTrace();
      }
    }

    if( !(orgMapModell instanceof GisTemplateMapModell) )
      throw new ExecutionException( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.SelectRoughnessThemeHandler.1" ) ); //$NON-NLS-1$

    final GisTemplateMapModell mapModell = (GisTemplateMapModell) orgMapModell;

    /* Find rougness layers to choose from */
    final IKalypsoTheme[] roughnessThemes = findRoughnessThemes( mapModell );

    /* ask user which rougness layer to activate */
    final IKalypsoTheme choosenTheme = showNetworksDialog( shell, roughnessThemes );
    if( choosenTheme == null )
      return Status.CANCEL_STATUS;

    /* Activate choosen layer */

    /* Check if this theme is already present, if true, just activate it */

    final CompositeCommand compositeCommand = new CompositeCommand( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.SelectRoughnessThemeHandler.2" ) ); //$NON-NLS-1$
    compositeCommand.addCommand( new EnableThemeCommand( choosenTheme, true ) );
    makeParentVisible( compositeCommand, choosenTheme );
    compositeCommand.addCommand( new ActivateThemeCommand( mapModell, choosenTheme ) );
    final ICommand command = compositeCommand;
    mapView.postCommand( command, null );

    return Status.OK_STATUS;
  }

  /**
   * Recursive helper that also makes all parent themes visible.
   */
  private void makeParentVisible( final CompositeCommand compositeCommand, final IKalypsoTheme theme )
  {
    final IMapModell parent = theme.getMapModell();
    if( parent != null )
    {
      final Object themeParent = parent.getThemeParent( theme );
      if( themeParent instanceof IKalypsoTheme )
      {
        final IKalypsoTheme parentTheme = (IKalypsoTheme) themeParent;
        compositeCommand.addCommand( new EnableThemeCommand( parentTheme, true ) );
        makeParentVisible( compositeCommand, parentTheme );
      }
    }
  }

  private IKalypsoTheme showNetworksDialog( final Shell shell, final IKalypsoTheme[] roughnessThemes )
  {
    final ListDialog dialog = new ListDialog( shell );
    dialog.setTitle( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.SelectRoughnessThemeHandler.3" ) ); //$NON-NLS-1$
    dialog.setMessage( Messages.getString( "org.kalypso.kalypso1d2d.pjt.actions.SelectRoughnessThemeHandler.4" ) ); //$NON-NLS-1$
    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IKalypsoTheme theme = (IKalypsoTheme) element;
        return theme.getLabel();
      }
    } );

    dialog.setInput( roughnessThemes );

    if( roughnessThemes.length > 0 )
      dialog.setInitialSelections( new Object[] { roughnessThemes[0] } );

    if( dialog.open() != Window.OK )
      return null;

    final Object[] result = dialog.getResult();
    if( result.length > 0 )
      return (IKalypsoTheme) result[0];

    return null;
  }

  private IKalypsoTheme[] findRoughnessThemes( final IMapModell mapModell )
  {
    final IKalypsoThemePredicate predicate = new IKalypsoThemePredicate()
    {
      @Override
      public boolean decide( final IKalypsoTheme theme )
      {
        if( theme == null )
          return false;
        if( !(theme instanceof IKalypsoFeatureTheme) )
          return false;
        final IFeatureType featureType = ((IKalypsoFeatureTheme) theme).getFeatureType();
        if( featureType == null )
          return false;
        final boolean equals = featureType.getQName().equals( RoughnessPolygon.FEATURE_ROUGHNESS_POLYGON );
        return equals;
      }
    };
    final KalypsoThemeVisitor visitor = new KalypsoThemeVisitor( predicate );
    mapModell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    return visitor.getFoundThemes();
  }

}
