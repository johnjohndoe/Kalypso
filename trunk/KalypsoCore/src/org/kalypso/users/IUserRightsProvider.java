package org.kalypso.users;

/**
 * IUserRightsProvider common interface for user rights providers. This interface
 * is used by the KalypsoUserService.
 * 
 * @author schlienger
 */
public interface IUserRightsProvider
{
  /**
   * Delivers the list of rights the given user has.
   * 
   * @param username
   * @return list of rights
   * @throws UserRightsException
   */
  public String[] getRights( final String username ) throws UserRightsException;
}
