package org.kalypso.model.wspm.ui.profil.operation;

import org.eclipse.core.commands.operations.IUndoContext;
import org.kalypso.model.wspm.core.profil.IProfil;


/**
 * UndoContext für Profil-Operationen.
 * Ein ProfilUndoContext passt (matches) einen anderen, wenn
 * beide das gleiche Profil repräsentieren.
 * 
 * @author Belger
 *
 */
public class ProfilUndoContext implements IUndoContext
{
  private final IProfil m_profil;

  public ProfilUndoContext( final IProfil profil )
  {
    m_profil = profil;
  }

  /**
   * @see org.eclipse.core.commands.operations.IUndoContext#getLabel()
   */
  public String getLabel( )
  {
    return "ProfilUndoContext";
  }

  /**
   * @see org.eclipse.core.commands.operations.IUndoContext#matches(org.eclipse.core.commands.operations.IUndoContext)
   */
  public boolean matches( final IUndoContext context )
  {
    return context instanceof ProfilUndoContext && m_profil == ((ProfilUndoContext)context).m_profil;
  }
}
