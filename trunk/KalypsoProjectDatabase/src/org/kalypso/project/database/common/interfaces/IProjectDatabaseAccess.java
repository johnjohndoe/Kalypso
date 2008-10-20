package org.kalypso.project.database.common.interfaces;

public interface IProjectDatabaseAccess
{
  /**
   * example url: String url = "webdav://planer:client@localhost:8888/webdav/incoming/test.zip";
   */

  /**
   * ProjectModelDatabase incoming directory protocol (webdav://, ftp://, etc)
   */
  public static final String INCOMING_PROTOCOL = "org.kalypso.project.database.incoming.protocol";

  /**
   * ProjectModelDatabase incoming directory user name (String)
   */
  public static final String INCOMING_USER_NAME = "org.kalypso.project.database.incoming.user.name";

  /**
   * ProjectModelDatabase incoming directory password (String)
   */
  public static final String INCOMING_PASSWORD = "org.kalypso.project.database.incoming.password";

  /**
   * ProjectModelDatabase incoming directory url (String - without protocol)
   */
  public static final String INCOMING_URL = "org.kalypso.project.database.incoming.url";

  public static final String PROJECT_BASE_PROTOCOL = "org.kalypso.project.database.base.protocol";

  public static final String PROJECT_BASE_USER_NAME = "org.kalypso.project.database.base.user.name";

  public static final String PROJECT_BASE_PASSWORD = "org.kalypso.project.database.base.password";

  public static final String PROJECT_BASE_URL = "org.kalypso.project.database.base.url";

  /**
   * @param local
   *          part of url - example: subDir/file.zip
   */
  String getUrl( String local );

}
