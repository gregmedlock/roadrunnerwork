<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
           xmlns:xs="http://www.w3.org/2001/XMLSchema">
<xsl:template match="/">
<html>
<head />
  <body title="Tests">
     <xsl:for-each select="unittest-results">
     <p>
     <xsl:for-each select="Suite">
     <xsl:if test="position( )=1">
          <table border="1">
          <thead>
          <tr>
                    <td>Suite</td>
                    <td>Name</td>
                    <td>Time</td>
          </tr>
       </thead>
       <tbody>
       <xsl:for-each select="../test">
           <tr>
           <td>
              <xsl:for-each select="@suite">
              <xsl:value-of select="." />
              </xsl:for-each>
         </td>
         <td>
              <xsl:for-each select="@name">
              <xsl:value-of select="." />
              </xsl:for-each>
        </td>
        <td>
              <xsl:for-each select="@time">
              <xsl:value-of select="." />
              </xsl:for-each>
        </td>
        </tr>
        </xsl:for-each>
      </tbody>
      </table>
      </xsl:if>
</xsl:for-each>
</p>
</xsl:for-each>
</body>
</html>
</xsl:template>
</xsl:stylesheet>

