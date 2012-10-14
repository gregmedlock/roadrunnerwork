<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
  <body>
  <h2>Tests</h2>
  <table border="1">
    <tr bgcolor="#9acd32">
      <th>Suite</th>
      <th>Test</th>
    </tr>
    <xsl:for-each select="unittest-results/cd">
    <tr>
      <td><xsl:value-of select="suite"/></td>
      <td><xsl:value-of select="name"/></td>
      <td><xsl:value-of select="time"/></td>
    </tr>
    </xsl:for-each>
  </table>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>


